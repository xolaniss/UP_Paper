# Description
# Pre-processes data for modelling - 2023-11-28 Xolani Sibande
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
# Import competition_narratives data
competition_narratives <- read_rds(here("Outputs", "narratives", "artifacts_competion_narratives.rds"))
competition_narratives_tbl <- competition_narratives$data$combined_dummies_tbl

# Import BA920 data
BA920 <- read_rds(here("Outputs", "BA920", "artifacts_BA920.rds"))
BA920_tbl <- BA920$data # How to we get this data to bank level?

# Import BA900 data
# BA900 <- read_rds(here("Outputs", "BA900", "artifacts_BA900.rds")) # Not clear how we should use this data

# Import BA930 data
BA930 <- read_rds(here("Outputs", "BA930", "artifacts_BA930.rds"))
BA930_tbl <- BA930$data$lending_rate_tbl

# Import controls data
controls <- read_rds(here("Outputs", "controls", "artifacts_controls.rds"))
controls_tbl <- controls$controls_tbl

# Adjusting narratives data for bank level --------------------------------------
group_bank <- function(data, Banks){
  data %>% 
    mutate(Banks = Banks) %>% 
    # relocate Banks after Date
    dplyr::select(Date, Banks, everything())
}

# stack using group_bank function
bank_level_competition_naratives_tbl <- 
  bind_rows(
  group_bank(competition_narratives_tbl, "Absa Bank"),
  group_bank(competition_narratives_tbl, "Capitec"),
  group_bank(competition_narratives_tbl, "FNB"),
  group_bank(competition_narratives_tbl, "Nedbank"),
  group_bank(competition_narratives_tbl, "Standard Bank")
) 

# Adjusting BA930 to wide form --------------------------------------------
# Drop outstanding balance from BA930_tbl
BA930_wide_tbl <- 
  BA930_tbl %>% 
  dplyr::select(-`Outstanding balance at month end R'000` , -Item) %>% 
  # Pivot to wide form
  pivot_wider(names_from = "Description", values_from = `Weighted average rate (%)`) %>% 
  # adjust date form from datetime to date
  mutate(Date = as.Date(Date))

# Adjusting controls to wide form -----------------------------------------
controls_wide_tbl <- 
  controls_tbl %>% 
  pivot_wider(names_from = "Series", values_from = "Value") %>% 
  # relocate Date to before Bank
  dplyr::select(Date, Bank, everything()) %>%
  # rename Bank to Banks
  rename(Banks = Bank) %>% 
  #convert Banks to title case
  mutate(Banks = str_to_title(Banks)) %>% 
  #rename Firstrand Bank to FNB
  mutate(Banks = str_replace_all(Banks, "Firstrand Bank", "FNB"))

# Combining data ----------------------------------------------------------
# merge bank_level_competition_narratives_tbl, BA930_wide_tbl, and controls_wide_tbl
competition_combined_tbl <- 
  bank_level_competition_naratives_tbl %>% 
  left_join(BA930_wide_tbl, by = c("Date", "Banks")) %>% 
  left_join(controls_wide_tbl, by = c("Date", "Banks"))

competition_combined_tbl %>% 
  skim() # check

# Export ---------------------------------------------------------------
artifacts_competition_preprocessing <- list (
  competition_combined_tbl = competition_combined_tbl
)

write_rds(artifacts_competition_preprocessing, 
          file = here("Outputs", "combined_data", "artifacts_competition_preprocessing.rds"))


