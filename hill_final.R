library(tidyverse)
library(haven)
library(fixest)
library(gt)
library(stringr)
library(glue)
library(modelsummary)
library(showtext)

# Data Cleaning -----------------------------------------------------------
## Read data
dat <- read_dta("data/final_main.dta")

## Choose variables with erroneous -99.0 values
variables_to_replace <- c("covihme_ayem", "covwdi_exp", "covwdi_fdi", "covwdi_imp", 
                          "covwvs_rel", "coviNY_GDP_PETR_RT_ZS", "covdemregion", 
                          "covloggdpC", "covloggdp")

## Mutate variables to replace -99.0 values with "NA"
dat <- dat |> 
  mutate_at(all_of(variables_to_replace), ~ replace(., . == -99, NA))

## Subset data to only include years after 1986 as in Carnegie and Marinov 2017
dat_subset <- filter(dat, year >= 1987)

## Download fonts for use in figures and tables
font_add_google(name = "Lato", family = "lato")
showtext_auto()

# Summary Statistics ------------------------------------------------------
dat_subset |> 
  select(EV, l2CPcol2, new_empinxavg, polity2avg, covihme_ayem, covwdi_exp, covwdi_fdi, covwdi_imp, covwvs_rel, coviNY_GDP_PETR_RT_ZS, covdemregion, covloggdp, covloggdpC) |> 
  pivot_longer(cols = everything()) |> 
  group_by(name) |> 
  summarize(
    mean = mean(value, na.rm = TRUE), 
    sd = sd(value, na.rm = TRUE), 
    n = sum(!is.na(value))
  ) |> 
  mutate(category = recode(
    name,
    EV = "Treatment", 
    l2CPcol2 = "Treatment", 
    new_empinxavg = "Outcome", 
    polity2avg = "Outcome",
    .default = "Covariate"
  )) |> 
  mutate(name = recode_factor(
    name,
    EV = "EU Aid", 
    l2CPcol2 = "Proportion of countries with former colony status, time t - 2", 
    new_empinxavg = "CIRI Human Empowerment Index (0 to 14), 4-year avg.", 
    polity2avg = "Polity IV score (-10 to 10), 4-year avg.", 
    covihme_ayem = "Avg. years education among men (Source: Gakidou et al. 2010)",
    covwvs_rel = "Religiosity (Source: Alesina et al. 2003)", 
    covdemregion = "Democracies in Region (Source: Marshall et al. 2002)", 
    covloggdp = "Log(GDP) (Source: WDI 2014)", 
    covloggdpC = "Log(GDP per capita) Source: WDI 2014)", 
    covwdi_exp = "Log(exports) (Source: WDI 2014)", 
    covwdi_imp = "Log(imports) (Source: WDI 2014)", 
    covwdi_fdi = "Log(FDI) (Source: WDI 2014)", 
    coviNY_GDP_PETR_RT_ZS = "Petroleum revenues (Source: WDI 2014)"
  )) |> 
  arrange(name) |>
  group_by(category) |> 
  gt() |> 
  cols_align("left", columns = name) |> 
  cols_align("center", columns = c(mean, sd, n)) |> 
  cols_label(
    name = "",
    mean = "Mean", 
    sd = "Std. Dev.", 
    n = "N"
  ) |> 
  fmt_number(columns = c(mean, sd), decimals = 2) |> 
  fmt_integer(columns = n) |> 
  tab_style(style = cell_text(weight = "bold"), 
            locations = cells_row_groups()) |> 
  tab_style(style = cell_text(weight = "bold"), 
            locations = cells_column_labels()) |> 
  opt_table_font(font = google_font("Lato")) |> 
  tab_options(table_body.hlines.width = px(0)) |> 
  gtsave(filename = "figures/sumstats.png")


# Replication of Table 1 --------------------------------------------------
## Create a vector of covariates for use in regression models
covs <- dat |> 
  colnames() |> 
  #str_subset("^cov.*(?<!F)$") |> 
  str_subset("cov") |> 
  str_c(collapse = " + ")

## Create vector of indicator variables that act as TWFE
indics <- dat |> 
  colnames() |> 
  str_subset("_I") |> 
  ## Wrap indicator variables in backticks for use in as.formula()
  sapply(function(x) str_c("`", x, "`")) |> 
  str_c(collapse = " + ")

## Define IV/TWFE  models
mod1_form <- as.formula(glue("new_empinxavg ~ {indics} | EV ~ l2CPcol2"))
mod2_form <- as.formula(glue("new_empinxavg ~ {indics} + {covs} | EV ~ l2CPcol2"))
mod3_form <- as.formula(glue("polity2avg ~ {indics} | EV ~ l2CPcol2"))
mod4_form <- as.formula(glue("polity2avg ~ {indics} + {covs} | EV ~ l2CPcol2"))

## Generate IV/TWFE models and store in list
models <- list(
  "mod1" = feols(mod1_form, dat_subset, cluster = c("ccode", "year")),
  "mod2" = feols(mod2_form, dat_subset, cluster = c("ccode", "year")),
  "mod3" = feols(mod3_form, dat_subset, cluster = c("ccode", "year")),
  "mod4" = feols(mod4_form, dat_subset, cluster = c("ccode", "year"))
)

## Calculate N, Countries, and Years in each regression 
## Define function to calculate statistics for filtered data
summarize_data <- function(df) {
  df |> 
  summarize(
    countries = n_distinct(ccode), 
    years = n_distinct(year), 
    n = n()
  )
}

## Filter dat_subset depending on the variables in each model
mod1_stats <- dat_subset |> 
  filter(is.na(new_empinxavg) != TRUE) |> 
  summarize_data()

mod2_stats <- dat_subset |> 
  filter(across(all_of(variables_to_replace), ~ !is.na(.))) |>
  filter(is.na(new_empinxavg) != TRUE) |> 
  summarize_data()

mod3_stats <- dat_subset |> 
  filter(is.na(polity2avg) != TRUE) |> 
  summarize_data()

mod4_stats <- dat_subset |> 
  filter(across(all_of(variables_to_replace), ~ !is.na(.))) |>
  filter(is.na(polity2avg) != TRUE) |> 
  summarize_data()

## Visualize regression results
table1 <- modelsummary(models,
             coef_map = c(fit_EV = "Effect of Aid"),
             gof_map = NA, 
             ## Hard code table data
             add_rows = data.frame(
               label = c("Countries", "Years", "Covariates", "Country Fixed Effects", "Year Fixed Effects", "N"),
               mod1 = c(mod1_stats$countries, mod1_stats$years, "No", "Yes", "Yes", mod1_stats$n),
               mod2 = c(mod2_stats$countries, mod2_stats$years, "Yes", "Yes", "Yes", mod2_stats$n),
               mod3 = c(mod3_stats$countries, mod3_stats$years, "No", "Yes", "Yes", mod3_stats$n),
               mod4 = c(mod4_stats$countries, mod4_stats$years, "Yes", "Yes", "Yes", mod4_stats$n)),
             output = "gt")

## Edit regression table using gt()
table1 |> 
  sub_values(columns = 1, rows = 2, values = "", replacement = "(Standard Error)") |> 
  tab_spanner(label = "Dependent Variable (4-Year Average)", columns = starts_with("mod"), level = 2) |> 
  tab_spanner(label = "CIRI Human Empowerment Index", columns = c(mod1, mod2), level = 1) |> 
  tab_spanner(label = "Polity IV Combined Score", columns = c(mod3, mod4), level = 1) |> 
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_spanners()) |> 
  tab_options(#column_labels.border.bottom.width = px(0), ## makes tab_spanner borders disappear
              table_body.hlines.width = px(0)) |> 
  tab_style(
    style = "padding-right:125px",
    locations = cells_column_labels()
  ) |> 
  tab_options(column_labels.padding = px(10)) |> 
  cols_label(starts_with("mod") ~ "") |>
  fmt_integer(rows = everything()) |> 
  opt_table_font(font = google_font("Lato"))
  

# Replication of Figure 1 -------------------------------------------------
## Create new lead variables for new_empinxavg and polity2avg 
## Generate for years t + 1 --> t + 5
for (i in 1:5) {
  new_empinx_lead <- str_c("new_empinx_", i, "_lead")
  polity2_lead <- str_c("polity2_", i, "_lead")
  dat_subset <- dat_subset |> 
    mutate(
      !!new_empinx_lead := lead(new_empinx, i),
      !!polity2_lead := lead(polity2, i)
    )
}

## Store polity2 and new_empinx and their leads for use in regression 
new_empinx_vars <- dat_subset |> 
  colnames() |> 
  str_subset("new_empinx(?!avg)")

polity2_vars <- dat_subset |> 
  colnames() |> 
  str_subset("polity2(?!avg)")

dep_vars_sets <- list(new_empinx_vars, polity2_vars)

## Initialize empty data frame to store regression results
leads <- tibble()

## Iterate over sets of new_empinx and polity2 variables
for (dep_vars_set in dep_vars_sets) {
  ## Iterate over lead variables within new_empinx and polity2 variable sets
  for (dep_var in dep_vars_set) {
    ## Generate IV/FE regression models
    mod_form <- as.formula(glue("{dep_var} ~ {indics} | EV ~ l2CPcol2"))
    
    ## Run regressions
    mod <- feols(mod_form, dat_subset, cluster = c("ccode", "year"))
    
    ## Extract point estimate and SE from regression 
    result <- tibble(dv = dep_var,
                     estimate = coef(mod)["fit_EV"],
                     se = se(mod)["fit_EV"])
    
    ## Append results to leads tibble
    leads <- bind_rows(leads, result)
  } 
}

## Clean data for use in event study plots
leads <- leads |> 
  mutate(set = case_when(
    dv %in% new_empinx_vars ~ "CIRI Human Empowerment Index", 
    TRUE ~ "Polity IV Score")) |> 
  group_by(set) |> 
  mutate(year = row_number() - 1)

## Event study plot for new_empinx
leads |>
  filter(set == "CIRI Human Empowerment Index") |>
  ggplot(aes(year, estimate)) +
  geom_point(shape = 15,
             size = 2.5,
             fill = "black") +
  geom_errorbar(aes(
    ymin = estimate - 1.96 * se,
    ymax = estimate + 1.96 * se,
    width = .10
  )) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-6, 6),
                     breaks = seq(-6, 6, by = 2)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(x = "Years Forward", y = "Effect of Foreign Aid")

## Event study plot for new_empinx
new_empinx_plot <- leads |>
  filter(set == "CIRI Human Empowerment Index") |>
  ggplot(aes(year, estimate)) +
  geom_point(shape = 15,
             size = 1.25,
             fill = "black") +
  geom_errorbar(aes(
    ymin = estimate - 1.96 * se,
    ymax = estimate + 1.96 * se,
    width = .10
  ),
  alpha = 0.25) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  scale_y_continuous(limits = c(-6, 6),
                     breaks = seq(-6, 6, by = 2)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(
      family = "lato",
      size = 32,
      hjust = 0.5
    ),
    axis.title = element_text(family = "lato", size = 24),
    axis.text = element_text(size = 20)
  ) +
  labs(title = "CIRI Human Empowerment Index",
       x = "Years Forward",
       y = "Effect of Foreign Aid")

ggsave("figures/fig1a.png", new_empinx_plot, height = 3, width = 3)

## Event study plot for polity2
polity2_plot <- leads |>
  filter(set == "Polity IV Score") |>
  ggplot(aes(year, estimate)) +
  geom_point(shape = 15,
             size = 1.25,
             fill = "black") +
  geom_errorbar(aes(
    ymin = estimate - 1.96 * se,
    ymax = estimate + 1.96 * se,
    width = .10
  ),
  alpha = 0.25) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  scale_y_continuous(limits = c(-10, 10),
                     breaks = seq(-10, 10, by = 5)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(
      family = "lato",
      size = 32,
      hjust = 0.5
    ),
    axis.title = element_text(family = "lato", size = 24),
    axis.text = element_text(size = 20)
  ) +
  labs(title = "Polity IV Score",
       x = "Years Forward",
       y = "Effect of Foreign Aid")

## Save plot
ggsave("figures/fig1b.png", polity2_plot, height = 3, width = 3)