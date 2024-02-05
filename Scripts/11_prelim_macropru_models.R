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
  clean_names()

# Modelling function -------------------------------------------------------------
response_vec <- c(
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

predictor_draft_vec <- c(
  "draft",
  "-1",
  "factor(banks)",
  "factor(month)"
)

# draft models -------------------------------------------------------------
draft_models <- 
  model_workflow(
  data = combined_tbl,
  response_vec = response_vec,
  predictor_vec = predictor_draft_vec
) 

modelsummary(
    draft_models,
    stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
    coef_omit = NULL
  )

# implementation models -------------------------------------------------------------
predictor_implementation_vec <- c(
  "implementation",
  "-1",
  "factor(banks)",
  "factor(month)"
)

implementation_models <- 
  model_workflow(
  data = combined_tbl,
  response_vec = response_vec,
  predictor_vec = predictor_implementation_vec
) 

modelsummary(
  implementation_models, 
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  coef_omit = NULL
  )

# Export ---------------------------------------------------------------
artifacts_macropru_models <- list (
  draft_models = draft_models,
  implementation_models = implementation_models
)

write_rds(artifacts_macropru_models, file = here("Outputs", "models", "artifacts_macropru_models.rds"))


