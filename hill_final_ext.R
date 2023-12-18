library(tidyverse)
library(haven)
library(fixest)
library(gt)
library(stringr)
library(glue)
library(modelsummary)
library(showtext)

# Data Cleaning and Merging ----------------------------------------------------
## Read data files
nunn <- read_dta("data/nunn_qian.dta")
prio <- read_csv("data/prio.csv")
cow <- read_csv("data/cow_ccodes.csv")

## Clean UCDP/PRIO conflict data
prio_test <- prio |> 
  ## Exclude conflicts in Europe and those before Carnegie and Marinov's analysis
  filter(year >= 1987 & region != 1) |> 
  ## Remove secondary country names, which differ from names in dat_subset
  mutate(location = gsub("\\([^\\)]+\\)", "", location)) |> 
  ## Change certain variable names
  mutate(location = case_when(
    location == "DR Congo " ~ "Democratic Republic of the Congo", 
    location == "Guinea-Bissau" ~ "Equatorial Guinea", 
    TRUE ~ str_trim(location)
  )) |> 
  ## For interstate conflicts, split up multiple participant countries
  separate(location, into = paste0("location", 1:4), sep = ", ", fill = "right") |> 
  pivot_longer(cols = starts_with("location"), 
               names_to = "location_order", 
               values_to = "location") |> 
  ## Remove rows with no conflict
  filter(!is.na(location)) |> 
  group_by(year, location) |> 
  filter(location %in% countries_in_dat_subset) |> 
  filter(duplicated(location) | duplicated(location, fromLast = TRUE)) |> 
  select(location, year, type_of_conflict, location_order) |> 
  arrange(location, year)
  
  
  



## Join to Carnegie and Marinov data
dat_subset <- dat_subset |> 
  left_join(cow, by = c("ccode" = "CCode")) |> 
  rename(country = StateNme) |> 
  left_join(nunn, by = c("country" = "recipient_country", "year"))

dat_test <- dat_subset |> 
  select(ccode, year) |> 
  left_join(distinct(cow, StateNme, CCode), by = c("ccode" = "CCode")) |> 
  rename(country = StateNme) |> 
  left_join(prio, by = c("country" = "location", "year"))











mod_form <- as.formula(glue("any_war ~ {indics} | EV ~ l2CPcol2"))
mod <- feols(mod_form, dat_subset, cluster = c("ccode", "year"))
modelsummary(mod, 
             coef_map = c("fit_EV" = "Effect of Aid"))



## Download fonts for use in figures and tables
font_add_google(name = "Lato", family = "lato")
showtext_auto()
dat_ext
