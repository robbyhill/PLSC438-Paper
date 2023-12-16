library(tidyverse)
library(haven)
library(fixest)
library(gt)
library(stringr)
library(glue)
library(modelsummary)
library(kableExtra)


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


# Summary Statistics ------------------------------------------------------
dat |> 
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
    sd = "Std. Dev."
  ) |> 
  fmt_number(columns = c(mean, sd), decimals = 2) |> 
  fmt_integer(columns = n) |> 
  tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) |> 
  gtsave("figures/sumstats.png")


# Replication of Table 1 --------------------------------------------------
## Create a vector of covariates for use in regression models
covs <- dat |> 
  colnames() |> 
  #str_subset("^cov.*(?<!F)$") |> 
  str_subset("cov") |> 
  str_c(collapse = " + ")

## Create vectors of country and year indicators to act as fixed effects
country_fe <- dat |> 
  colnames() |> 
  str_subset("_Iccode") |> 
  ## Wrap indicator variables in backticks for use in as.formula()
  sapply(function(x) str_c("`", x, "`")) |> 
  str_c(collapse = " + ")

year_fe <- dat |> 
  colnames() |> 
  str_subset("_Iyear") |> 
  ## Wrap indicator variables in backticks for use in as.formula()
  sapply(function(x) str_c("`", x, "`")) |> 
  str_c(collapse = " + ")

## Subset data to only include years after 1986 
dat_subset <- filter(dat, year >= 1987)

## Define regression models
mod1_form <- as.formula(glue("new_empinxavg ~ {country_fe} + {year_fe} | EV ~ l2CPcol2"))
mod2_form <- as.formula(glue("new_empinxavg ~ {country_fe} + {year_fe} + {covs} | EV ~ l2CPcol2"))
mod3_form <- as.formula(glue("polity2avg ~ {country_fe} + {year_fe} | EV ~ l2CPcol2"))
mod4_form <- as.formula(glue("polity2avg ~ {country_fe} + {year_fe} + {covs} | EV ~ l2CPcol2"))

## Generate models
mod1 <- feols(mod1_form, dat_subset, cluster = c("ccode", "year"))
mod2 <- feols(mod2_form, dat_subset, cluster = c("ccode", "year"))
mod3 <- feols(mod3_form, dat_subset, cluster = c("ccode", "year"))
mod4 <- feols(mod4_form, dat_subset, cluster = c("ccode", "year"))

## Calculate statistics to add to the bottom of the regression table
## Each of the models includes different variables in the regression, so I filter
## across those variables to check for missing values and then calculate the
## number of countries and years included in the regression
mod1_stats <- dat_subset |> 
  filter(is.na(new_empinxavg) != TRUE) |> 
  summarize(
    years = n_distinct(year), 
    countries = n_distinct(ccode)
  )

mod2_stats <- dat_subset |> 
  filter(across(all_of(variables_to_replace), ~ !is.na(.))) |>
  filter(is.na(new_empinxavg) != TRUE) |> 
  summarize(
    years = n_distinct(year), 
    countries = n_distinct(ccode)
  )

mod3_stats <- dat_subset |> 
  filter(is.na(polity2avg) != TRUE) |> 
  summarize(
    years = n_distinct(year), 
    countries = n_distinct(ccode)
  )

mod4_stats <- dat_subset |> 
  filter(across(all_of(variables_to_replace), ~ !is.na(.))) |>
  filter(is.na(polity2avg) != TRUE) |> 
  summarize(
    years = n_distinct(year), 
    countries = n_distinct(ccode)
  )

## Visualize regression table using modelsummary()
models <- list(
  "mod1" = mod1,
  "mod2" = mod2,
  "mod3" = mod3,
  "mod4" = mod4
)

table1 <- modelsummary(models,
             coef_map = c(fit_EV = "Effect of Aid"),
             gof_map = c("nobs"), 
             ## Hard code table data
             add_rows = data.frame(
               label = c("Countries", "Years", "Covariates", "Year Fixed Effects", "Country Fixed Effects"),
               mod1 = c(mod1_stats$countries, mod1_stats$years, "No", "Yes", "Yes"),
               mod2 = c(mod2_stats$countries, mod2_stats$years, "Yes", "Yes", "Yes"),
               mod3 = c(mod3_stats$countries, mod3_stats$years, "No", "Yes", "Yes"),
               mod4 = c(mod4_stats$countries, mod4_stats$years, "Yes", "Yes", "Yes")),
             output = "gt")

## Edit regression table using gt()
table1 |> 
  tab_spanner(label = "Dependent Variable (4-Year Average)", columns = 1, id = "spanner1") |> 
  tab_spanner(label = "CIRI Human Empowerment Index", columns = c(mod1, mod2), id = "spanner2") |> 
  tab_spanner(label = "Polity IV Combined Score", columns = c(mod3, mod4), id = "spanner3") |> 
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_spanners()) |> 
  tab_style(style = cell_borders(sides = c("bottom"), weight = px(0)),
            locations = cells_column_spanners()) |> 
  cols_label(mod1 = "", 
             mod2 = "", 
             mod3 = "",
             mod4 = "") 


# Replication of Figure 1 -------------------------------------------------
