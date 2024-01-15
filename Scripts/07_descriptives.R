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

# Import -------------------------------------------------------------
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

# Descriptives --------------------------------------------------------
descriptives_lending_tbl <- 
  aggregated_lending_tbl %>% 
  filter(Banks == "Total Banks") %>%
  dplyr::select(-Banks) %>% 
  group_by(Series) %>% 
  summarise(across(.cols = -c(Date),
                   .fns = list(Median = median, 
                               SD = sd,
                               Min = min,
                               Max = max,
                               IQR = IQR,
                               Obs = ~ n()), 
                   .names = "{.fn}")) %>% 
  mutate(Group = "Lending")


descriptives_lending_rates_tbl <- 
  aggregated_lending_rates_tbl %>% 
  filter(Banks == "Total Banks") %>% 
  dplyr::select(-Banks) %>% 
  group_by(Series) %>% 
  summarise(across(.cols = -c(Date),
                   .fns = list(Median = median, 
                               SD = sd,
                               Min = min,
                               Max = max,
                               IQR = IQR,
                               Obs = ~ n()), 
                   .names = "{.fn}")) %>% 
  mutate(Group = "Lending Rates")

descriptives_control_tbl <- 
  controls_tbl %>%
  drop_na() %>%
  dplyr::select(-Banks) %>%
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
  mutate(Group = "Controls")


descriptives_tbl <- 
  bind_rows(
    descriptives_lending_tbl,
    descriptives_lending_rates_tbl,
    descriptives_control_tbl
  )

# Export ---------------------------------------------------------------
artifacts_descriptives <- list (
  data = list(
    aggregated_lending_tbl = aggregated_lending_tbl,
    aggregated_lending_rates_tbl = aggregated_lending_rates_tbl,
    controls_tbl = controls_tbl
  ),
  desc = list(
    descriptives_tbl = descriptives_tbl
  )
)

write_rds(artifacts_descriptives, file = here("Outputs", "combined_data", "artifacts_descriptives.rds"))


