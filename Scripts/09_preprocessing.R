# Description
# preliminary models Xolani Sibande - 2023
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

# Import -------------------------------------------------------------
combined <- read_rds(here("Outputs", "combined_data", "artifacts_descriptives.rds"))
aggregate_lending_tbl <- combined$data$aggregated_lending_tbl %>% 
  pivot_wider(id_cols = c(Date, Banks),
              names_from = "Series", 
              values_from = "Value")
aggregate_lending_rates_tbl <- combined$data$aggregated_lending_rates_tbl %>% 
  pivot_wider(id_cols = c(Date, Banks), 
              names_from = "Series", 
              values_from = "Value")
macropru_tbl <- combined$data$macropru_index_tbl %>% 
  pivot_wider(names_from = "Series", values_from = "Value")
competition_index <- read_rds(here("Outputs", 
                                   "narratives", 
                                   "artifacts_competion_narratives.rds"))
competition_tbl <- competition_index$data$combined_dummies_tbl %>% 
  dplyr::select(-c(Event, Description))
competition_tbl %>% skim()

controls <- read_rds(here("Outputs", "controls", "artifacts_controls.rds"))
controls_tbl <- controls$controls_tbl
controls_tbl
# Combined data -------------------------------------------------------------
## Combining narratives  -------------------------------------------------------------
dummies_prelim_tbl <- 
  macropru_tbl %>% 
  left_join(competition_tbl, by = c("Date")) # combined dummies

dummies_tbl <- 
  list(
  "Total Banks" = dummies_prelim_tbl,
  "Absa Bank" = dummies_prelim_tbl,
  "FNB" = dummies_prelim_tbl,
  "Nedbank" = dummies_prelim_tbl,
  "Standard Bank" = dummies_prelim_tbl,
  "Capitec" = dummies_prelim_tbl
  ) %>% 
  bind_rows(.id = "Banks") %>% 
  relocate(Banks, .after = Date)

## Combining lending and lending rates -------------------------------------------------------------
lending_capitec_nan_tbl <- 
  aggregate_lending_tbl %>% 
  left_join(aggregate_lending_rates_tbl, by = c("Date", "Banks")) # combined lending

lending_tbl <- 
  # We decided to replace capitec NaNs with 0s. Because of the division of the shares. This is essential for the exit and entry story. To delete capitec in the overall analysis because of "missing" or zero values.
  lending_capitec_nan_tbl %>% 
  mutate(across(everything(), ~replace(.x, is.nan(.x), 0))) %>% 
  drop_na() # because of date difference between lending data and lending rates data
 
## Checks -------------------------------------------------------------
lending_tbl %>% group_by(Banks) %>% skim() 
dummies_tbl %>% group_by(Banks) %>% skim() 

## Combining lending and lending rates and dummies -------------------------------------------------------------
combined_tbl <- 
  lending_tbl %>% 
  left_join(dummies_tbl, by = c("Date", "Banks")) # combined lending and dummies
combined_tbl %>% group_by(Banks) %>% skim()

## Combining lending and lending rates and dummies and controls -------------------------------------------------------------
# Still to do

# Transformations --------------------------------------------------------


# EDA ---------------------------------------------------------------


# Graphing ---------------------------------------------------------------


# Export ---------------------------------------------------------------
artifacts_ <- list (

)

write_rds(artifacts_, file = here("Outputs", "artifacts_.rds"))


