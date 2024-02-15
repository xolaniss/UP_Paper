# Description
# Preliminary models for paper - Xolani Sibande 5 February 2023

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
  dplyr::filter(Banks != "Total Banks") %>%
  mutate(covid_dummy = if_else(Date >= "2020-03-01", 1, 0)) %>% 
  clean_names() %>% 
  filter(date >= "2009-01-01" & date < "2020-03-01") # Covid adjustment

# Three lending models -------------------------------------------------------------

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
  "factor(banks)",
  "factor(month)"
)

three_month_draft_lending_models <- 
  model_workflow(
    data = combined_tbl,
    response_vec = response_lending_draft_vec,
    predictor_vec = predictor_lending_draft_vec
  )

modelsummary(
  three_month_draft_lending_models,
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  coef_omit = NULL
)

## implementation  -------------------------------------------------------------
predictor_implementation_lending_vec <- c(
  "implementation",
  "-1",
  # "covid_dummy",
  "factor(banks)",
  "factor(month)"
)

three_month_implementation_lending_models <- 
  model_workflow(
    data = combined_tbl,
    response_vec = response_lending_draft_vec,
    predictor_vec = predictor_implementation_lending_vec
  )

modelsummary(
  three_month_implementation_lending_models,
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  coef_omit = NULL
)


# Export ---------------------------------------------------------------
artifacts_three_month_macropru_models <- list (
  three_lending_models = list(
    three_month_draft = three_month_draft_lending_models,
    three_month_implementation = three_month_implementation_lending_models
  )
)

write_rds(artifacts_three_month_macropru_models, file = here("Outputs", "models", "artifacts_three_month_macropru_models.rds"))


