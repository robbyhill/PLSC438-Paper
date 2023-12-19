library(tidyverse)
library(haven)
library(fixest)
library(gt)
library(stringr)
library(glue)
library(modelsummary)
library(showtext)
library(giscoR)

# Data Cleaning and Merging ----------------------------------------------------
## Read data files
nunn <- read_dta("data/nunn_qian.dta")
prio <- read_csv("data/prio.csv")
cow <- read_csv("data/cow_ccodes.csv")

## Clean UCDP/PRIO conflict data
prio <- prio |> 
  ## Exclude conflicts in Europe and those before Carnegie and Marinov's analysis
  filter(year >= 1987 & region != 1) |> 
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
  ## Summarize data to merge later
  group_by(year, location) |> 
  summarize(
    prio_conflict_count = n(),
    prio_major_conflict_count = sum(intensity_level == 2), 
    prio_minor_conflict_count = sum(intensity_level == 1), 
    prio_conflict_binary = as.numeric(prio_conflict_count >= 1), 
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

## Left join CoW and UCDP/PRIO data frames to dat_subset by country, year
dat_subset <- dat |>
  filter(year >= 1987) |> 
  left_join(distinct(cow, StateNme, CCode), by = c("ccode" = "CCode")) |> 
  rename(country = StateNme) |> 
  left_join(prio, by = c("country" = "location", "year")) |> 
  ## Replace NA values with 0s 
  mutate_at(all_of(prio_vars), ~ ifelse(is.na(.), 0, .))
  

# Extension Regression Table ----------------------------------------------
## Generate regression formulas
reg1_form <- as.formula(glue("prio_conflict_binary ~ {indics} + EV"))
reg2_form <- as.formula(glue("prio_conflict_binary ~ {indics} | EV ~ l2CPcol2"))
reg3_form <- as.formula(glue("prio_any_intra ~ {indics} + EV"))
reg4_form <- as.formula(glue("prio_any_intra ~ {indics} | EV ~ l2CPcol2"))
reg5_form <- as.formula(glue("prio_any_inter ~ {indics} + EV"))
reg6_form <- as.formula(glue("prio_any_inter ~ {indics} | EV ~ l2CPcol2"))

## Run regression
regs <- list (
  "reg1" = feols(reg1_form, dat_subset, cluster = c("country", "year")),
  "reg2" = feols(reg2_form, dat_subset, cluster = c("country", "year")),
  "reg3" = feols(reg3_form, dat_subset, cluster = c("country", "year")),
  "reg4" = feols(reg4_form, dat_subset, cluster = c("country", "year")),
  "reg5" = feols(reg5_form, dat_subset, cluster = c("country", "year")),
  "reg6" = feols(reg6_form, dat_subset, cluster = c("country", "year"))
)

modelsummary(regs, 
             coef_map = c("EV" = "Effect of Aid", 
                          "fit_EV" = "Effect of Aid"), 
             stars = TRUE)



# Extension Figure --------------------------------------------------------
dat_subset <- dat_subset |> 
  mutate(region = case_when(
    ccode < 200 ~ "Americas", 
    ccode < 400 ~ "Europe", 
    ccode < 627 ~ "Africa", 
    ccode < 700 ~ "Middle East", 
    TRUE ~ "Asia"
  ))


shp <- gisco_get_countries(region = "Africa")



