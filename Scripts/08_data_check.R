# Description
# Summary table for paper by Xolani Sibande - 15 January 2024
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
# 
# Import -------------------------------------------------------------
macropru_index <- read_rds(here("Outputs", 
                                "narratives", 
                                "artifacts_macropru_narratives.rds"))
macropru_index_tbl <- macropru_index$macropru_narratives_tbl %>% 
  pivot_longer(cols = -Date, names_to = "Series", values_to = "Value")
macropru_index_tbl

competition_index <- read_rds(here("Outputs", 
                                   "narratives", 
                                   "artifacts_competion_narratives.rds"))
competition_index_tbl <- competition_index$data$combined_dummies_tbl %>% 
  dplyr::select(-c(Event, Description)) %>%
  pivot_longer(cols = -Date, names_to = "Series", values_to = "Value")
competition_index_tbl

lending <- read_rds(here("Outputs", "BA900", "artifacts_BA900.rds"))
aggregated_lending_tbl <- lending$aggregated_data$combined_aggregated_lending_tbl
aggregated_lending_tbl 

lending_rates <- read_rds(here("Outputs", "BA930", "artifacts_BA930_futher_analysis.rds"))
aggregated_lending_rates_tbl <- lending_rates$data$aggregated_lending_rates_tbl %>% 
  pivot_longer(cols = -c(Date, Banks), names_to = "Series", values_to = "Value")
aggregated_lending_rates_tbl 

controls <- read_rds(here("Outputs", "BA920", "artifacts_BA920.rds"))
controls_tbl <- 
  controls$data$Total_banks_tbl %>%  
  mutate(Banks = "Total Banks") %>% 
  relocate(Date, .before = Series)
controls_tbl

# data_check --------------------------------------------------------
banks_data_check <- function (data, group_var){
  data %>% 
    drop_na() %>%
    group_by(Series) %>%
    summarise(across(
      .cols = -c(Date),
      .fns = list(
        Median = median,
        SD = sd,
        Min = min,
        Max = max,
        IQR = IQR,
        Obs = ~ n()
      ),
      .names = "{.fn}"
    )) %>%
    mutate(Group = group_var)
  }

data_check_lending_tbl <- 
  aggregated_lending_tbl %>% 
  filter(Banks == "Total Banks") %>% 
  dplyr::select(-Banks) %>%
  banks_data_check(group_var = "Lending")
  
data_check_lending_rates_tbl <- 
  aggregated_lending_rates_tbl %>% 
  filter(Banks == "Total Banks") %>% 
  dplyr::select(-Banks) %>%
  banks_data_check(group_var = "Lending Rates")
  
data_check_control_tbl <- 
  controls_tbl %>%
  filter(Banks == "Total Banks") %>% 
  dplyr::select(-Banks) %>%
  banks_data_check(group_var = "Controls")

data_check_macropru_tbl <-
  macropru_index_tbl %>% 
  banks_data_check(group_var = "Macroprudential Narrative Indices")

data_check_competition_index_tbl <-
  competition_index_tbl %>% 
  banks_data_check(group_var = "Competition Narrative Indices")

data_check_tbl <- 
  bind_rows(
    data_check_macropru_tbl,
    competition_index_tbl,
    data_check_lending_tbl,
    data_check_lending_rates_tbl,
    data_check_control_tbl
  )

# Export ---------------------------------------------------------------
artifacts_data_check <- list (
  data = list(
    aggregated_lending_tbl = aggregated_lending_tbl,
    aggregated_lending_rates_tbl = aggregated_lending_rates_tbl,
    controls_tbl = controls_tbl,
    macropru_index_tbl = macropru_index_tbl,
    competition_index_tbl = competition_index_tbl
  ),
  desc = list(
    data_check_tbl = data_check_tbl
  )
)

write_rds(artifacts_data_check, file = here("Outputs", "combined_data", "artifacts_data_check.rds"))


