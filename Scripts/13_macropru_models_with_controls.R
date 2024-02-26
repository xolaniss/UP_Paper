# Description
# Controls models for macroprudential - Xolani Sibande 15 February 2023

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
library(modelsummary)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
source(here("Functions", "formular_function.R"))
source(here("Functions", "model_workflow.R"))

# Import -------------------------------------------------------------
combined <- read_rds(here("Outputs", "combined_data", "artifacts_modelling_data.rds"))
combined_tbl <- 
  combined$combined_features_tbl %>% 
  mutate(month = lubridate::month(Date)) %>%
  mutate(year = lubridate::year(Date)) %>%
  dplyr::filter(Banks != "Total Banks") %>%
  mutate(covid_dummy = if_else(Date >= "2020-03-01", 1, 0)) %>% 
  clean_names() %>% 
  filter(date >= "2009-01-01" & date < "2020-03-01") # Covid adjustment

combined_tbl %>% glimpse()
# Rates models ---------------------------------------------------------------

## draft  -------------------------------------------------------------
response_rates_draft_vec <- c(
  "Total unsecured lending rate",
  "Total leasing and installments rate",
  "Total mortgages lending rate",
  "Non financial corporate unsecured lending rate",
  "Leasing and installements to corporate rate",
  "Commercial mortgages to corporates and households rate",
  "Household unsecured lending rate",
  "Leasing and installments to households rate",
  "Residential mortgages to household rate"
)

predictor_rates_draft_vec <- c(
  "draft",
  "-1",
  # "covid_dummy",
  "return_on_assets",
  "total_capital_adequacy_ratio",
  # "impairments",
  "factor(banks)",
  "factor(month)"
  # "factor(year)"
)

draft_rates_controls_models <- 
  model_workflow(
    data = combined_tbl,
    response_vec = response_rates_draft_vec,
    predictor_vec = predictor_rates_draft_vec
  ) 

modelsummary(
  draft_rates_controls_models,
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  coef_omit = NULL
)

## implementation  -------------------------------------------------------------
predictor_implementation_rates_vec <- c(
  "implementation",
  "-1",
  # "covid_dummy",
  "return_on_assets",
  "total_capital_adequacy_ratio",
  # "impairments",
  "factor(banks)",
  "factor(month)"
  # "factor(year)"
)

implementation_rates_controls_models <- 
  model_workflow(
    data = combined_tbl,
    response_vec = response_rates_draft_vec,
    predictor_vec = predictor_implementation_rates_vec
  ) 

modelsummary(
  implementation_rates_controls_models, 
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  coef_omit = NULL
)


# Lending models -------------------------------------------------------------

## draft  -------------------------------------------------------------
response_lending_draft_vec <- c(
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

predictor_lending_draft_vec <- c(
  "draft",
  "-1",
  # "covid_dummy",
  "return_on_assets",
  "total_capital_adequacy_ratio",
  # "impairments",
  "factor(banks)",
  "factor(month)"
  # "factor(year)"
)

draft_lending_controls_models <- 
  model_workflow(
    data = combined_tbl,
    response_vec = response_lending_draft_vec,
    predictor_vec = predictor_lending_draft_vec
  )

modelsummary(
  draft_lending_controls_models,
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  coef_omit = NULL
)

## implementation  -------------------------------------------------------------
predictor_implementation_lending_vec <- c(
  "implementation",
  "-1",
  # "covid_dummy",
  "return_on_assets",
  "total_capital_adequacy_ratio",
  # "impairments",
  "factor(banks)",
  "factor(month)"
  # "factor(year)"
)

implementation_lending_controls_models <- 
  model_workflow(
    data = combined_tbl,
    response_vec = response_lending_draft_vec,
    predictor_vec = predictor_implementation_lending_vec
  )

modelsummary(
  implementation_lending_controls_models,
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  coef_omit = NULL
)


# Export ---------------------------------------------------------------
artifacts_macropru_controls_models <- list (
  rates_models = list(
    draft = draft_rates_controls_models,
    implementation = implementation_rates_controls_models
  ),
  lending_models = list(
    draft = draft_lending_controls_models,
    implementation = implementation_lending_controls_models
  )
)

write_rds(artifacts_macropru_controls_models, file = here("Outputs", "models", "artifacts_macropru_controls_models.rds"))


