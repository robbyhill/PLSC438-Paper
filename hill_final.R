library(tidyverse)
library(haven)
library(fixest)
library(gt)
library(stringr)
library(glue)
library(modelsummary)

# ---- Clean data
## Read data
dat <- read_dta("data/final_main.dta")

## Choose variables with erroneous -99.0 values
variables_to_replace <- c("covihme_ayem", "covwdi_exp", "covwdi_fdi", "covwdi_imp", 
                          "covwvs_rel", "coviNY_GDP_PETR_RT_ZS", "covdemregion", 
                          "covloggdpC", "covloggdp")

## Mutate variables to replace -99.0 values with "NA"
dat <- dat |> 
  mutate_at(all_of(variables_to_replace), ~ replace(., . == -99, NA))

# Table 1: Summary Statistics ----
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


# Table 2: Main Results ----
## Create a vector of covariates for use in regression models
## Regex matches all strings beginning with "cov" but not ending in "F". ChatGPT assisted.
covs <- dat |> 
  colnames() |> 
  str_subset("^cov.*(?<!F)$") |> 
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
mod3_form <- update(mod1_form, polity2avg ~ .)
mod4_form <- update(mod2_form, polity2avg ~ .)

## Generate models
mod1 <- feols(mod1_form, dat_subset, cluster = c("ccode", "year"))
mod2 <- feols(mod2_form, dat_subset, cluster = c("ccode", "year"))
mod3 <- feols(mod3_form, dat_susbset, cluster = c("ccode", "year"))
mod4 <- feols(mod4_form, dat_subset, cluster = c("ccode", "year"))

## Visualize models
mods <- list(mod1, mod2, mod3, mod4)
modelsummary(mods, 
             coef_map = c(
               fit_EV = "Effect of Aid"
             ))
## First Stage Regression ----







# Figure 1: Event Study Plot ----
