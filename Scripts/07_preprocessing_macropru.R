# Description
# Pre-processes data for modelling - 2023-11-27 Xolani Sibande

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
library(janitor)
library(stringr)

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
# Import macropru_narratives data
macropru_narratives <- read_rds(here("Outputs", "narratives", "artifacts_macropru_narratives.rds"))
macropru_narratives_tbl <- macropru_narratives$macropru_narratives_tbl

# Import BA900 data
# BA900 <- read_rds(here("Outputs", "BA900", "artifacts_BA900.rds")) # Not clear how we should use this data

# Import BA930 data
BA930 <- read_rds(here("Outputs", "BA930", "artifacts_BA930.rds"))
BA930_tbl <- BA930$data$lending_rate_tbl

# Import controls data
controls <- read_rds(here("Outputs", "controls", "artifacts_controls.rds"))
controls_tbl <- controls$controls_tbl

# Adjusting the narratives to bank level ----------------------------------
group_bank <- function(data, Banks){
  data %>% 
    mutate(Banks = Banks) %>% 
    # relocate Banks after Date
    dplyr::select(Date, Banks, everything())
}

bank_level_narratives_tbl <-
  bind_rows(
    macropru_narratives_tbl %>%
      group_bank(Banks = "Total Banks"),
    macropru_narratives_tbl %>%
      group_bank(Banks = "Standard Bank"),
    macropru_narratives_tbl %>%
      group_bank(Banks = "FNB"),
    macropru_narratives_tbl %>%
      group_bank(Banks = "Absa Bank"),
    macropru_narratives_tbl %>%
      group_bank(Banks = "Nedbank"),
    macropru_narratives_tbl %>%
      group_bank(Banks = "Capitec")
  )

bank_level_narratives_tbl %>% glimpse()
  
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

controls_wide_tbl$Banks %>% unique()
# Combining  ----------------------------------------
# Joining narratives with BA930_wide_tbl and controls_wide_tbl
macro_pru_combined_tbl <- 
  bank_level_narratives_tbl %>% 
  left_join(controls_wide_tbl, by = c("Date" = "Date", "Banks" = "Banks")) %>% 
  left_join(BA930_wide_tbl, by = c("Date" = "Date", "Banks" = "Banks")) %>% 
  # relocate Banks after Date
  dplyr::select(Date, Banks, everything()) 

macro_pru_combined_tbl %>% 
  filter(Banks == "Absa Bank") %>% 
  print(n = 100)


# Export ---------------------------------------------------------------
artifacts_macropru_preprocessing <- list (
  macro_pru_combined_tbl = macro_pru_combined_tbl
)

write_rds(artifacts_macropru_preprocessing, file = here("Outputs", "combined_data", "artifacts_macropru_preprocessing.rds"))


