library(tidyverse)
library(haven)
library(fixest)
library(gt)

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
dat <- dat |> 
  mutate(
    
  )
## First Stage Regression ----







# Figure 1: Event Study Plot ----
