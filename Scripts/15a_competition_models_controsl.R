# Description
# new financial regulation models - Xolani Sibande 14 March 2024

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
library(janitor)

# econometrics
library(tseries)
library(strucchange)
library(vars)
library(urca)
library(mFilter)
library(car)
options(scipen = 999)
# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
source(here("Functions", "formular_function.R"))
source(here("Functions", "model_workflow.R"))
source(here("Functions", "excel_import_sheet.R"))

# Import -------------------------------------------------------------
combined <- read_rds(here("Outputs", "combined_data", "artifacts_combined_spreads.rds"))
combined_tbl <- 
  combined$combined_spread_tbl %>% 
  mutate(month = lubridate::month(Date)) %>%
  mutate(covid_dummy = if_else(Date >= "2020-03-01", 1, 0)) %>%  # Covid dummy
  dplyr::filter(Banks != "Total Banks") %>%
  clean_names() %>% 
  filter(date >= "2009-01-01" & date < "2020-03-01") # Covid adjustment
combined_tbl %>% skim()

combined_tbl$consumer_confidence_index
combined_tbl$savit40
combined_tbl$return_on_assets
combined_tbl$repo
# Finance regulation ------------------------------------------------------

## Rates models ------------------------------------------------------------
response_finance_rates_vec <- c(
  "total_unsecured_lending_rate",
  "total_leasing_and_installments_rate",
  "total_mortgages_lending_rate",
  "non_financial_corporate_unsecured_lending_rate",
  "leasing_and_installements_to_corporate_rate",
  "commercial_mortgages_to_corporates_and_households_rate",
  "household_unsecured_lending_rate",
  "leasing_and_installments_to_households_rate",
  "residential_mortgages_to_household_rate"
)


predictor_finance_rates_vec <- c(
  "-1",
  "finance_regulation_dummy",
  # "covid_dummy",
  # "non_financial_corporate_unsecured_lending_spread",
  "repo",
  # "consumer_confidence_index",
  "return_on_assets",
  # "savit40",
  "factor(banks)",
  "factor(month)"
)    

finance_rates_models <- model_workflow(
  data = combined_tbl,
  response_vec = response_finance_rates_vec,
  predictor_vec = predictor_finance_rates_vec
)

finance_rates_models

## Lending models ----------------------------------------------------------
response_finance_lending_vec <- c(
  "three_month_change_in_log_total_unsecured_lending",
  "three_month_change_in_log_total_leasing_and_installments",
  "three_month_change_in_log_total_mortgage_lending",
  "three_month_change_in_log_non_financial_corporate_unsecured_lending",
  "three_month_change_in_log_leasing_and_installments_to_corporates",
  "three_month_change_in_log_commercial_mortgages_to_corporates_and_households",
  "three_month_change_in_log_household_unsecured_lending",
  "three_month_change_in_log_leasing_and_installments_to_households",
  "three_month_change_in_log_residential_mortgages_to_households"
)


predictor_finance_lending_vec <- c(
  "-1",
  "finance_regulation_dummy",
  # "covid_dummy",
  "repo",
  # "consumer_confidence_index",
  "return_on_assets",
  # "savit40",
  "factor(banks)",
  "factor(month)"
)

finance_lending_models <- model_workflow(
  data = combined_tbl,
  response_vec = response_finance_lending_vec,
  predictor_vec = predictor_finance_lending_vec
)

finance_lending_models


# Financial inclusion -----------------------------------------------------

## Rates models ------------------------------------------------------------

response_financial_inclusion_rates_vec <- c(
  "total_unsecured_lending_rate",
  "total_leasing_and_installments_rate",
  "total_mortgages_lending_rate",
  "non_financial_corporate_unsecured_lending_rate",
  "leasing_and_installements_to_corporate_rate",
  "commercial_mortgages_to_corporates_and_households_rate",
  "household_unsecured_lending_rate",
  "leasing_and_installments_to_households_rate",
  "residential_mortgages_to_household_rate"
)

predictor_financial_inclusion_rates_vec <- c(
  "-1",
  "financial_inclusion_dummy",
  "covid_dummy",
  "repo",
  "consumer_confidence_index",
  "return_on_assets",
  "savit40",
  "factor(banks)",
  "factor(month)"
)

financial_inclusion_rates_models <- 
  model_workflow(
  data = combined_tbl,
  response_vec = response_financial_inclusion_rates_vec,
  predictor_vec = predictor_financial_inclusion_rates_vec
)

## Lending models ----------------------------------------------------------
response_financial_inclusion_lending_vec <- c(
  "three_month_change_in_log_total_unsecured_lending",
  "three_month_change_in_log_total_leasing_and_installments",
  "three_month_change_in_log_total_mortgage_lending",
  "three_month_change_in_log_non_financial_corporate_unsecured_lending",
  "three_month_change_in_log_leasing_and_installments_to_corporates",
  "three_month_change_in_log_commercial_mortgages_to_corporates_and_households",
  "three_month_change_in_log_household_unsecured_lending",
  "three_month_change_in_log_leasing_and_installments_to_households",
  "three_month_change_in_log_residential_mortgages_to_households"
)

predictor_financial_inclusion_lending_vec <- c(
  "-1",
  "financial_inclusion_dummy",
  "covid_dummy",
  "repo",
  "consumer_confidence_index",
  "return_on_assets",
  "savit40",
  "factor(banks)",
  "factor(month)"
)

financial_inclusion_lending_models <- 
  model_workflow(
  data = combined_tbl,
  response_vec = response_financial_inclusion_lending_vec,
  predictor_vec = predictor_financial_inclusion_lending_vec
)

# Export ---------------------------------------------------------------
artifacts_additional_competition_control_models <- list (
  finance_rates_models = finance_rates_models,
  finance_lending_models = finance_lending_models,
  financial_inclusion_rates_models = financial_inclusion_rates_models,
  financial_inclusion_lending_models = financial_inclusion_lending_models
)

write_rds(artifacts_additional_competition_control_models, 
          file = here("Outputs", "models", "artifacts_additional_competition_control_models.rds"))


