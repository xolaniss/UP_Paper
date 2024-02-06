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

# Import -------------------------------------------------------------
combined <- read_rds(here("Outputs", "combined_data", "artifacts_modelling_data.rds"))
combined_tbl <- 
  combined$combined_features_tbl

combined_tbl %>% glimpse()

# Filtering ---------------------------------------------------------------
combined_filtered_tbl <- 
  combined_tbl %>% 
  filter(Banks == "Total Banks") 

# Summarising -------------------------------------------------------------
descriptives <- function (data, group_var){
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

descriptives_change_in_tbl <- 
  combined_filtered_tbl %>% 
  dplyr::select(
    Date,
    starts_with("Change_in")) %>% 
  pivot_longer(
    cols = -Date,
    names_to = "Series",
    values_to = "Value"
  ) %>% 
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
  pivot_longer(
    cols = -Date,
    names_to = "Series",
    values_to = "Value"
  ) %>% 
  descriptives(group_var = "Competition regulation narrative indices")

descriptives_tbl <- 
  bind_rows(
    descriptives_change_in_tbl,
    descriptives_lending_rates_tbl,
    descriptives_macropru_tbl,
    descriptives_competition_tbl
  )

# Export ---------------------------------------------------------------
artifacts_descriptives <- list (
  descriptives_tbl = descriptives_tbl
)

write_rds(artifacts_descriptives, file = here("Outputs","combined_data", "artifacts_descriptives.rds"))


