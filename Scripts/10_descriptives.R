# Description
# descriptives table for the paper
# Preliminaries -----------------------------------------------------------
# core
library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(xts)
library(broom)
library(glue)
library(scales)
library(kableExtra)
library(pins)
library(timetk)
library(uniqtag)
library(quantmod)

# graphs
library(PNWColors)
library(patchwork)

# eda
library(psych)
library(DataExplorer)
library(skimr)

# econometrics
library(tseries)
library(strucchange)
library(vars)
library(urca)
library(mFilter)
library(car)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
source(here("Functions", "descriptives.R"))

# Import -------------------------------------------------------------
combined <- read_rds(here("Outputs", "combined_data", "artifacts_modelling_data.rds"))
combined_tbl <- 
  combined$combined_features_tbl

combined_tbl

# Filtering ---------------------------------------------------------------
combined_filtered_tbl <- 
  combined_tbl %>% 
  filter(Banks == "Total Banks") 
combined_filtered_tbl %>% glimpse()
combined_filtered_tbl %>% skim()
# Summarising -------------------------------------------------------------

descriptives_change_in_tbl <- 
  combined_filtered_tbl %>% 
  dplyr::select(
    Date,
    starts_with("three_month_change")) %>% 
  pivot_longer(
    cols = -Date,
    names_to = "Series",
    values_to = "Value"
  ) %>% 
  mutate(Series = str_replace_all(Series, "_", " ")) %>%
  mutate(Series = str_to_sentence(Series)) %>% 
  descriptives(group_var = "Lending growth")
descriptives_lending_rates_tbl <- 
combined_filtered_tbl %>% 
  dplyr::select(
    Date,
    ends_with("rate")) %>%
  pivot_longer(
    cols = -Date,
    names_to = "Series",
    values_to = "Value"
  ) %>% 
  descriptives(group_var = "Lending rates")
descriptives_macropru_tbl <- 
  combined_filtered_tbl %>% 
  dplyr::select(
    Date,
    Draft, 
    Implementation) %>% 
  rename(
    "Draft Index" = "Draft",
    "Implementation Index" = "Implementation"
  ) %>% 
  pivot_longer(
    cols = -Date,
    names_to = "Series",
    values_to = "Value"
  ) %>% 
  descriptives(group_var = "Macroprudential regulation narrative indices")

descriptives_competition_tbl <- 
  combined_filtered_tbl %>% 
  dplyr::select(
    Date,
    ends_with("dummy")) %>% 
  rename(
    "All entry index" = Entry_all_dummy,
    "All exit index" = Exit_all_dummy,
    "Household entry index" = Entry_Household_dummy,
    "Household exit index" = Exit_Household_dummy,
    "Corporate entry index" = Entry_Corporate_dummy,
    "Corporate exit index" = Exit_Corporate_dummy,
    "All credit entry entry index" = Entry_credit_non_fin_dummy,
    "All credit entry exit index" = Exit_credit_non_fin_dummy,
    "Corporate credit entry index" = Entry_credit_non_fin_corporate_dummy,
    "Corporate credit exit index" = Exit_credit_non_fin_corporate_dummy,
    "Household credit entry index" = Entry_credit_households_dummy,
    "Household credit exit index" = Exit_credit_households_dummy,
    "Commercial mortgages entry" = Entry_commercial_mortgages_dummy,
    "Commercial mortgages exit" = Exit_commercial_mortgages_dummy,
    "Entry mortgages households index" = Entry_mortgages_households_dummy,
    "Exit mortgages households index" = Exit_mortgages_households_dummy,
    "Household leasing entry index" = Entry_leasing_households_dummy,
    "Household leasing exit index" = Exit_leasing_households_dummy,
    "Corporate leasing entry index" = Entry_leasing_non_fin_corporate_dummy,
    "Finance regulation index" = finance_regulation_dummy,
    "Other competition index" = competition_dummy,
    "Financial inclusion index" = financial_inclusion_dummy
  ) %>% 
  pivot_longer(
    cols = -Date,
    names_to = "Series",
    values_to = "Value"
  ) %>% 
  descriptives(group_var = "Competition regulation narrative indices")

descriptives_controls_tbl <- 
  combined_filtered_tbl %>%
  dplyr::select(
    Date,
    repo,
    `Consumer confidence index`,
    SAVIT40,
    `Return on assets`,
    `Total capital adequacy ratio`
  ) %>%
  rename(
    "Repo rate" = repo,
    "Consumer confidence index" = `Consumer confidence index`,
    "SAVIT40 index" = SAVIT40,
  ) %>% 
  pivot_longer(
    cols = -Date,
    names_to = "Series",
    values_to = "Value"
  ) %>%
  descriptives(group_var = "Controls")

descriptives_tbl <- 
  bind_rows(
    descriptives_change_in_tbl,
    descriptives_lending_rates_tbl,
    descriptives_macropru_tbl,
    descriptives_competition_tbl,
    descriptives_controls_tbl
  ) 



# Export ---------------------------------------------------------------
artifacts_descriptives <- list (
  descriptives_tbl = descriptives_tbl
)

write_rds(artifacts_descriptives, file = here("Outputs","combined_data", "artifacts_descriptives.rds"))


