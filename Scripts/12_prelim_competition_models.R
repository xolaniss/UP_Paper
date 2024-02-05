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

# econometrics
library(tseries)
library(strucchange)
library(vars)
library(urca)
library(mFilter)
library(car)

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
  clean_names()

# Entry Exit unsecured corporations models -------------------------------------------------------------
response_unsecured_corp_vec <- c(
  "Non financial corporate unsecured lending rate"
)

predictor_unsecured_corp_vec <- c(
  "-1",
  "entry_credit_non_fin_dummy",
  "exit_credit_non_fin_dummy",
  "factor(banks)",
  "factor(month)"
)

entry_exit_corp_model <- 
  model_workflow(
  data = combined_tbl,
  response_vec = response_unsecured_corp_vec,
  predictor_vec = predictor_unsecured_corp_vec
)

entry_exit_corp_model

# Entry Exit unsecured households models -------------------------------------------------------------
response_unsecured_household_vec <- c(
  "Household unsecured lending rate"
)

predictor_unsecured_household_vec <- c(
  "-1",
  "entry_credit_households_dummy",
  "exit_credit_households_dummy",
  "factor(banks)",
  "factor(month)"
)

entry_exit_household_model <- 
  model_workflow(
  data = combined_tbl,
  response_vec = response_unsecured_household_vec,
  predictor_vec = predictor_unsecured_household_vec
)

entry_exit_household_model

# Export ---------------------------------------------------------------
artifacts_competition_models <- list (
  entry_exit_corp_model = entry_exit_corp_model,
  entry_exit_household_model = entry_exit_household_model
)

write_rds(artifacts_competition_models, 
          file = here("Outputs", "artifacts_competition_models.rds"))


