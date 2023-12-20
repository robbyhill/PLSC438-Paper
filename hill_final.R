library(tidyverse)
library(haven)
library(fixest)
library(gt)
library(stringr)
library(glue)
library(modelsummary)
library(showtext)
library(giscoR)
library(maps)
library(ggthemes)

# Data Cleaning -----------------------------------------------------------
## Read data
dat <- read_dta("data/final_main.dta")
nunn <- read_dta("data/nunn_qian.dta")
prio <- read_csv("data/prio.csv")
cow <- read_csv("data/cow_ccodes.csv")
iso <- read_csv("data/cow2iso.csv")
load("data/rugged.rda")

## Clean UCDP/PRIO conflict data
prio <- prio |> 
  ## Exclude conflicts in Europe and those before Carnegie and Marinov's analysis
  filter(year >= 1987) |> 
  ## Remove secondary country names, which differ from names in dat_subset
  mutate(location = gsub("\\([^\\)]+\\)", "", location)) |> 
  ## Change certain variable names
  mutate(location = case_when(
    location == "DR Congo " ~ "Democratic Republic of the Congo", 
    TRUE ~ str_trim(location)
  )) |> 
  ## For interstate conflicts, split up multiple participant countries
  separate(location, into = paste0("location", 1:4), sep = ", ", fill = "right") |> 
  pivot_longer(cols = starts_with("location"), 
               names_to = "location_order", 
               values_to = "location") |> 
  ## Remove rows with no conflict
  filter(!is.na(location)) |> 
  ## Summarize data and create key variables
  group_by(year, location) |> 
  summarize(
    prio_conflict_count = n(),
    prio_major_conflict_count = sum(intensity_level == 2),
    prio_minor_conflict_count = sum(intensity_level == 1),
    prio_any_conflict = as.numeric(prio_conflict_count >= 1),
    prio_any_intra = as.numeric(sum(type_of_conflict != 2) > 0),
    prio_any_inter = as.numeric(sum(type_of_conflict == 2) > 0),
    prio_intra_intensity = sum((type_of_conflict != 2) * intensity_level),
    prio_inter_intensity = sum((type_of_conflict == 2) * intensity_level),
    prio_max_intensity = ifelse(max(intensity_level) == 2, 2, 1)
  )

## Store new UCDP/PRIO variables in a vector to clean
prio_vars <- prio |> 
  colnames() |> 
  str_subset("prio_")

## Subset data to include only years after 1986 as in Carnegie and Marinov 2017
dat_subset <- dat |>
  filter(year >= 1987) |>
  ## Left join CoW, CoW-to-ISO, and UCDP/PRIO data frames to dat_subset by country, year
  left_join(distinct(cow, StateNme, CCode), by = c("ccode" = "CCode")) |>
  rename(country = StateNme) |>
  left_join(distinct(iso, cow_id, iso3), by = c("ccode" = "cow_id")) |>
  left_join(distinct(rugged, ccode, rugged), by = "ccode") |> 
  left_join(prio, by = c("country" = "location", "year")) |>
  ## Replace NA values with 0s
  mutate_at(all_of(prio_vars), ~ ifelse(is.na(.), 0, .)) |>
  ## Create onset variables
  ## Onset variable gets 1 iff prio_any_conflict == 1 in year t and != 1 in year t - 1
  group_by(country) |>
  mutate(
    region = case_when(
      ccode < 200 ~ "Americas", 
      ccode < 400 ~ "Europe", 
      ccode < 627 ~ "Africa", 
      ccode < 700 ~ "Middle East", 
      TRUE ~ "Asia"
    ), 
    prio_conflict_onset = as.numeric(prio_any_conflict == 1 &
                                       lag(prio_any_conflict, default = NA) != 1),
    prio_intra_onset = as.numeric(prio_any_intra == 1 &
                                    lag(prio_any_intra, default = NA) != 1),
    prio_inter_onset = as.numeric(prio_any_inter == 1 &
                                    lag(prio_any_inter, default = NA) != 1),
    prio_conflict_offset = as.numeric(prio_any_conflict == 0 &
                                        lag(prio_any_conflict, default = NA) == 1),
    prio_intra_offset = as.numeric(prio_any_intra == 0 &
                                        lag(prio_any_intra, default = NA) == 1),
    prio_inter_offset = as.numeric(prio_any_inter == 0 &
                                     lag(prio_any_inter, default = NA) == 1),
  ) |> 
  ungroup()

## Download fonts for use in figures and tables
font_add_google(name = "Lato", family = "lato")
showtext_auto()

## Clean covariates with erroneous -99.0 values
covs <- dat_subset |> 
  colnames() |> 
  str_subset("cov")



# Table 1: Summary Statistics ---------------------------------------------

## Mutate variables to replace -99.0 values with "NA"
dat_summary <- dat_subset |> 
  mutate_at(all_of(covs), ~ replace(., . == -99, NA))

## Summarize data
dat_summary <- dat_summary |> 
  select(
    EV,
    l2CPcol2,
    new_empinxavg,
    polity2avg,
    prio_any_conflict,
    prio_conflict_onset,
    prio_conflict_offset, 
    covihme_ayem,
    covwdi_exp,
    covwdi_fdi,
    covwdi_imp,
    covwvs_rel,
    coviNY_GDP_PETR_RT_ZS,
    covdemregion,
    covloggdp,
    covloggdpC
  ) |>
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
    new_empinxavg = "Measures of Human Rights and Democracy", 
    polity2avg = "Measures of Human Rights and Democracy",
    prio_any_conflict = "Measures of Civil Conflict (25+ deaths)",
    prio_conflict_onset = "Measures of Civil Conflict (25+ deaths)", 
    prio_conflict_offset = "Measures of Civil Conflict (25+ deaths)",
    .default = "Covariate"
  )) |> 
  mutate(name = recode_factor(
    name,
    EV = "EU Aid",
    prio_any_conflict = "Any conflict", 
    prio_conflict_onset = "Conflict onset",
    prio_conflict_offset = "Conflict offset",
    l2CPcol2 = "Countries with former colony status, time t - 2", 
    new_empinxavg = "CIRI Human Empowerment Index (0 to 14), 4-year avg.", 
    polity2avg = "Polity IV score (-10 to 10), 4-year avg.",
    covihme_ayem = "Avg. years education among men",
    covwvs_rel = "Religiosity score (0 to 10)", 
    covdemregion = "Prop. of countries in region that are democracies", 
    covloggdp = "Log(GDP) (2014 USD)", 
    covloggdpC = "Log(GDP per capita) (2014 USD)", 
    covwdi_exp = "Log(exports) (% of GDP)", 
    covwdi_imp = "Log(imports) (% of GDP)", 
    covwdi_fdi = "FDI (% of GDP)", 
    coviNY_GDP_PETR_RT_ZS = "Petroleum revenues (% of GDP)"
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
  tab_options( data_row.padding = px(2.5)) |> 
  tab_source_note(
    source_note = "Notes: An observation is a country and year. The sample includes 115 countries and covers the years 1987 to 2007. 'EU Aid' is calculated as logged net EU official development assistance (ODA; in millions of 1995 USD), time t - 1"  
  )

dat_summary |> gtsave("paper/tables/sumstats.tex", expand = 30)


# Table 2 - Regression Table ----------------------------------------------

## Create a vector of covariates for use in regression models
covs <- covs |> str_c(collapse = " + ")

## Create vector of indicator variables that act as TWFE
indics <- dat_subset |> 
  colnames() |> 
  str_subset("_I") |> 
  ## Wrap indicator variables in backticks for use in as.formula()
  sapply(function(x) str_c("`", x, "`")) |> 
  str_c(collapse = " + ")

## Define IV/TWFE  replication models
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

## Define function to calculate N, countries, and years in each regression
summarize_data <- function(df) {
  df |> 
    summarize(
      countries = n_distinct(ccode), 
      years = n_distinct(year), 
      n = n()
    )
}

covs <- dat_subset |> 
  colnames() |> 
  str_subset("cov")

## Filter dat_subset depending on the variables in each model
mod1_stats <- dat_subset |> 
  filter(is.na(new_empinxavg) != TRUE) |> 
  summarize_data()

mod2_stats <- dat_subset |> 
  filter(across(all_of(covs), ~ !is.na(.))) |>
  filter(is.na(new_empinxavg) != TRUE) |> 
  summarize_data()

mod3_stats <- dat_subset |> 
  filter(is.na(polity2avg) != TRUE) |> 
  summarize_data()

mod4_stats <- dat_subset |> 
  filter(across(all_of(covs), ~ !is.na(.))) |>
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
table1 <- table1 |> 
  fmt_integer(rows = 8) |> 
  sub_values(columns = 1, rows = 2, values = "", replacement = "(Standard Error)") |> 
  tab_spanner(label = "Dependent Variable (4-Year Average)", columns = starts_with("mod"), level = 2) |> 
  tab_spanner(label = "CIRI Human Empowerment Index", columns = c(mod1, mod2), level = 1) |> 
  tab_spanner(label = "Polity IV Combined Score", columns = c(mod3, mod4), level = 1) |> 
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_spanners()) |> 
  tab_options(table_body.hlines.width = px(0)) |> 
  cols_label(starts_with("mod") ~ "") |>
  opt_table_font(font = google_font("Lato")) |> 
  tab_source_note(source_not = "Note: 'Covariates' include Average Years Education, Religiosity, Democracies in Region, Log GDP, Log GDP p.c., Log Exports, Log Imports, FDI, Petroleum Revenues, and the two-year lag of . Standard errors are clustered at the country and year levels. The first-stage coefficient on Colony is 0.160 (SE = 0.074) for the CIRI regression and 0.154 (SE = 0.087) for the Polity regression.")

table1 |> gtsave("paper/tables/replication.tex")


# Table 3 - Extension --------------------------------------------------
## Create democracy index controls
demos <- char("lag(new_empinx, 2) + lag(polity2, 2)")

## Filter data to keep time frame the same as the replication
dat_conflict <- dat_subset |> filter(year <= 2007)

## Generate IV/TWFE regression formulas
reg1_form <- as.formula(glue("prio_any_conflict ~ {indics} + {covs} + {demos} + EV"))
reg2_form <- as.formula(glue("prio_any_conflict ~ {indics} + {covs} + {demos} | EV ~ l2CPcol2"))
reg3_form <- as.formula(glue("prio_conflict_onset ~ {indics} + {covs} + {demos} + EV"))
reg4_form <- as.formula(glue("prio_conflict_onset ~ {indics} + {covs} + {demos} | EV ~ l2CPcol2"))
reg5_form <- as.formula(glue("prio_conflict_offset ~ {indics} + {covs} + {demos} + EV"))
reg6_form <- as.formula(glue("prio_conflict_offset ~ {indics} + {covs} + {demos} | EV ~ l2CPcol2"))

## Run regression
regs <- list (
  "1ols" = feols(reg1_form, dat_conflict, cluster = c("country", "year")),
  "1iv" = feols(reg2_form, dat_subset, cluster = c("country", "year")),
  "2ols" = feols(reg3_form, dat_subset, cluster = c("country", "year")),
  "2iv" = feols(reg4_form, dat_subset, cluster = c("country", "year")),
  "3ols" = feols(reg5_form, dat_subset, cluster = c("country", "year")),
  "3iv" = feols(reg6_form, dat_subset, cluster = c("country", "year"))
)

## Visualize regression results
table2 <- modelsummary(
  regs,
  coef_map = c("EV" = "Effect of Aid",
               "fit_EV" = "Effect of Aid"),
  ## Hard code table data
  add_rows = data.frame(
    label = c(
      "Covariates",
      "Country Fixed Effects",
      "Year Fixed Effects"
    ),
    reg1 = c("Yes", "Yes", "Yes"),
    reg2 = c("Yes", "Yes", "Yes"),
    reg3 = c("Yes", "Yes", "Yes"),
    reg4 = c("Yes", "Yes", "Yes"), 
    reg5 = c("Yes", "Yes", "Yes"),
    reg6 = c("Yes", "Yes", "Yes")
  ),
  gof_map = "nobs",
  output = "gt"
)

table2 <- table2 |> 
  fmt_integer(rows = 3) |> 
  sub_values(columns = 1, rows = 2, values = "", replacement = "(Standard Error)") |> 
  tab_spanner(label = "Incidence", columns = starts_with("1"), level = 1) |> 
  tab_spanner(label = "Onset", columns = starts_with("2"), level = 1) |> 
  tab_spanner(label = "Offset", columns = starts_with("3"), level = 1) |> 
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_spanners()) |> 
  tab_options(table_body.hlines.width = px(0)) |> 
  tab_style(
    style = "padding-right:75px",
    locations = cells_column_labels()
  ) |> 
  cols_label(ends_with("ols") ~ "OLS", 
             ends_with("iv") ~ "2SLS") |>
  opt_table_font(font = google_font("Lato")) |> 
  cols_align(align = "center", columns = c(2:7)) |> 
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = everything())) |> 
  tab_source_note(source_note = "Note: 'Covariates' include Average Years Education, Religiosity, Democracies in Region, Log GDP, Log GDP p.c., Log Exports, Log Imports, FDI, Petroleum Revenues, twice-lagged Polity IV score, and twice-lagged CIRI Human Empowerment score. Standard errors are clustered at the country and year levels.")

table2 |> gtsave("paper/tables/extension.tex")

# Figure 1: Small Multiples Map -------------------------------------------

## Get African countries shapefile data
africa <- gisco_get_countries(region = "Africa")

## Create panel data for all African countries to address missing values in dat_subset
africa_panel <- expand.grid(ISO3_CODE = unique(africa$ISO3_CODE),
                            year = seq(1987, 2007))

## Join africa shapefile with panel data and dat_subset by country and year
africa <- africa |> 
  left_join(africa_panel, by = "ISO3_CODE") |>
  left_join(dat_subset |> filter(year <= 2007),
            by = c("ISO3_CODE" = "iso3", "year" = "year"))

## Map conflict incidence data 
africa |>
  group_by(NAME_ENGL) |> 
  ## Replace missing values
  mutate(prio_any_conflict = ifelse(is.na(prio_any_conflict), -1, prio_any_conflict)) |>
  mutate(prio_any_conflict = factor(
    recode(prio_any_conflict,
           "-1" = "Did not receive EU aid",
           "0" = "No conflict",
           "1" = "Conflict"),
    levels = c("Conflict", "No conflict", "Did not receive EU aid")
  )) |>
  ggplot(aes(fill = prio_any_conflict)) +
  geom_sf() +
  scale_fill_manual(values = c("Did not receive EU aid" = "white", "No conflict" = "gray", "Conflict" = "#ed5151")) +
  labs(fill = "",
       caption = "Source: UCDP/PRIO Version 23.1") +
  ## Use facet_wrap to track change in conflict incidence over time
  facet_wrap( ~ year, nrow = 5) +
  theme_map() +
  theme(
    legend.position = c(.72, .00),
    legend.text = element_text(size = 20, family = "lato"),
    strip.text = element_text(size = 20, family = "lato"),
    plot.caption = element_text(size = 15, family = "lato")
  )

## Save image
ggsave("paper/figures/africa.png", width = 5, height = 5.5)



# Appendix Figure 1: Event Study Plot -------------------------------------

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

ggsave("paper/figures/fig1a.png", new_empinx_plot, height = 3, width = 3)

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
ggsave("paper/figures/fig1b.png", polity2_plot, height = 3, width = 3)