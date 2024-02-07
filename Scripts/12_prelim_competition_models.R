# Description
# preliminary models for competiton - Xolani Sibande 5 February 2024

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

# Import -------------------------------------------------------------
combined <- read_rds(here("Outputs", "combined_data", "artifacts_modelling_data.rds"))
combined_tbl <- 
  combined$combined_features_tbl %>% 
  mutate(month = lubridate::month(Date)) %>%
  mutate(covid_dummy = if_else(Date >= "2020-03-01", 1, 0)) %>%  # Covid dummy
  dplyr::filter(Banks != "Total Banks") %>%
  clean_names() 
combined_tbl %>% glimpse()


# Entry Exit models -------------------------------------------------------------
## Unsecured corporations models -------------------------------------------------------------
response_unsecured_corp_vec <- c(
  "Non financial corporate unsecured lending rate",
  "Change_in_log_Non financial corporate unsecured lending"
)

predictor_unsecured_corp_vec <- c(
  "-1",
  "entry_credit_non_fin_dummy",
  "exit_credit_non_fin_dummy",
  "covid_dummy",
  "factor(banks)",
  "factor(month)"
)

corporate_unsecured_model <- 
  model_workflow(
  data = combined_tbl,
  response_vec = response_unsecured_corp_vec,
  predictor_vec = predictor_unsecured_corp_vec
)

corporate_unsecured_model

## Unsecured households models -------------------------------------------------------------
response_unsecured_household_vec <- c(
  "Household unsecured lending rate",
  "Change_in_log_Household unsecured lending"
)

predictor_unsecured_household_vec <- c(
  "-1",
  "entry_credit_households_dummy",
  "exit_credit_households_dummy",
  "covid_dummy",
  "factor(banks)",
  "factor(month)"
)

household_unsecured_model <- 
  model_workflow(
  data = combined_tbl,
  response_vec = response_unsecured_household_vec,
  predictor_vec = predictor_unsecured_household_vec
)

household_unsecured_model

## Commercial mortgages models -------------------------------------------------------------
response_commercial_mortgages_vec <- c(
  "commercial_mortgages_to_corporates_and_households_rate",
  "change_in_log_commercial_mortgages_to_corporates_and_households"
)

predictor_commercial_mortgages_vec <- c(
  "-1",
  "entry_commercial_mortgages_dummy",
  "exit_commercial_mortgages_dummy",
  "covid_dummy",
  "factor(banks)",
  "factor(month)"
)

commercial_mortgages_model <- 
  model_workflow(
  data = combined_tbl,
  response_vec = response_commercial_mortgages_vec,
  predictor_vec = predictor_commercial_mortgages_vec
)

commercial_mortgages_model

## Households mortgages models -------------------------------------------------------------
response_households_mortgages_vec <- c(
  "residential_mortgages_to_household_rate",
  "change_in_log_residential_mortgages_to_households"
)

predictor_households_mortgages_vec <- c(
  "-1",
  "entry_mortgages_households_dummy",
  "exit_mortgages_households_dummy",
  "covid_dummy",
  "factor(banks)",
  "factor(month)"
)

households_mortgages_model <- 
  model_workflow(
  data = combined_tbl,
  response_vec = response_households_mortgages_vec,
  predictor_vec = predictor_households_mortgages_vec
)

households_mortgages_model

## Household leasing and installments models -------------------------------------------------------------
response_household_leasing_installments_vec <- c(
  "leasing_and_installments_to_households_rate",
  "change_in_log_leasing_and_installments_to_households"
)

predictor_household_leasing_installments_vec <- c(
  "-1",
  "entry_leasing_households_dummy",
  "exit_leasing_households_dummy",
  "covid_dummy",
  "factor(banks)",
  "factor(month)"
)

household_leasing_installments_model <- 
  model_workflow(
  data = combined_tbl,
  response_vec = response_household_leasing_installments_vec,
  predictor_vec = predictor_household_leasing_installments_vec
)

household_leasing_installments_model

## Corporate leasing and installments models -------------------------------------------------------------
response_corporate_leasing_installments_vec <- c(
  "leasing_and_installements_to_corporate_rate",
  "change_in_log_leasing_and_installments_to_corporates"
)

predictor_corporate_leasing_installments_vec <- c(
  "-1",
  "entry_leasing_non_fin_corporate_dummy",
  "covid_dummy",
  "factor(banks)",
  "factor(month)"
)

coorporate_leasing_installments_model <- 
  model_workflow(
  data = combined_tbl,
  response_vec = response_corporate_leasing_installments_vec,
  predictor_vec = predictor_corporate_leasing_installments_vec
)

coorporate_leasing_installments_model

# Other competition models -------------------------------------------------------------
response_other_competition_vec <- c(
  "total_unsecured_lending_rate",
  "total_leasing_and_installments_rate",
  "total_mortgages_lending_rate",
  "change_in_log_total_unsecured_lending",
  "change_in_log_total_leasing_and_installments",
  "change_in_log_total_mortgage_lending"
)

predictor_other_competition_vec <- c(
  "-1",
  "competition_dummy",
  "covid_dummy",
  "factor(banks)",
  "factor(month)"
)

other_competition_model <- 
  model_workflow(
  data = combined_tbl,
  response_vec = response_other_competition_vec,
  predictor_vec = predictor_other_competition_vec
)

other_competition_model

# Finance regulations models -------------------------------------------------------------
response_finance_regulations_vec <- c(
  "total_unsecured_lending_rate",
  "total_leasing_and_installments_rate",
  "total_mortgages_lending_rate",
  "change_in_log_total_unsecured_lending",
  "change_in_log_total_leasing_and_installments",
  "change_in_log_total_mortgage_lending"
)

predictor_finance_regulations_vec <- c(
  "-1",
  "finance_regulation_dummy",
  "covid_dummy",
  "factor(banks)",
  "factor(month)"
)

finance_regulations_model <- 
  model_workflow(
  data = combined_tbl,
  response_vec = response_finance_regulations_vec,
  predictor_vec = predictor_finance_regulations_vec
)

finance_regulations_model

# Financial inclusion models -------------------------------------------------------------
response_financial_inclusion_vec <- c(
  "total_unsecured_lending_rate",
  "total_leasing_and_installments_rate",
  "total_mortgages_lending_rate",
  "change_in_log_total_unsecured_lending",
  "change_in_log_total_leasing_and_installments",
  "change_in_log_total_mortgage_lending"
)

predictor_financial_inclusion_vec <- c(
  "-1",
  "financial_inclusion_dummy",
  "covid_dummy",
  "factor(banks)",
  "factor(month)"
)

financial_inclusion_model <- 
  model_workflow(
  data = combined_tbl,
  response_vec = response_financial_inclusion_vec,
  predictor_vec = predictor_financial_inclusion_vec
)

financial_inclusion_model

# Export ---------------------------------------------------------------
artifacts_competition_models <- list (
  household_unsecured_model = household_unsecured_model,
  corporate_unsecured_model = corporate_unsecured_model,
  commercial_mortgages_model = commercial_mortgages_model,
  households_mortgages_model = households_mortgages_model,
  household_leasing_installments_model = household_leasing_installments_model,
  coorporate_leasing_installments_model = coorporate_leasing_installments_model,
  other_competition_model = other_competition_model,
  finance_regulations_model = finance_regulations_model,
  financial_inclusion_model = financial_inclusion_model
)

write_rds(artifacts_competition_models, 
          file = here("Outputs", "models", "artifacts_competition_models.rds"))


