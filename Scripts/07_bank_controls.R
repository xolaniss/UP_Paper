# Description
# bank controls for paper - Xolani Sibande 18 January 2023
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
source(here("Functions", "excel_import_sheet_controls.R"))

# Import -------------------------------------------------------------
sheet_list <-  list(
  "ABSA BANK"  = "Absa",
  "FIRSTRAND BANK" = "FRB",
  "NEDBANK" = "Nedbank",
  "STANDARD BANK" = "Std bank",
  "CAPITEC" = "Capitec")

path <- here("Data", "controls",  "ERD_data_request.xlsx")

controls <- 
  excel_import_sheet_controls(path = path, 
                              sheet_list = sheet_list, skip = 4) 


# Transformations --------------------------------------------------------
controls_tbl <- 
  controls %>% 
  map(~as.data.frame(t(.), check.names = TRUE)) %>% 
  map(~as_tibble(.)) %>% 
  map(~dplyr::select(., -V1, -V5, -V9)) %>% 
  map(~rename(.,
              "Total assets" = "V2",
              "Gross loan advances" = "V3",
              "Retained earnings" = "V4",
              "Level one high-quality liquid assets required to be held" = "V6",
              "Average daily amount of level one high-quality liquid assets" = "V7",
              "Aggregate risk weighted exposure" = "V8",
              "Return on equity" = "V10",
              "Return on assets"  = "V11",
              "Total capital adequacy ratio" = "V12",
              "Leverage ratio" = "V13")) %>% 
  map(~slice(., -c(1:3))) %>% 
  map(~mutate(., Date = seq(as.Date("2008-01-01"), as.Date("2022-09-01"), by = "month"))) %>% 
  map(~relocate(., Date, .before = `Total assets`)) %>% 
  map(~mutate(., across(-Date, .fns = ~as.numeric(.x)))) %>% 
  map(~pivot_longer(., -Date, names_to = "Series", values_to = "Value")) %>% 
  bind_rows(.id = "Bank")

# EDA ---------------------------------------------------------------
controls_tbl %>% group_by(Series, Bank) %>% skim()


# Graphing ---------------------------------------------------------------
# not important

# Export ---------------------------------------------------------------
artifacts_controls <- list (
  controls_tbl = controls_tbl
)

write_rds(artifacts_controls, file = here("Outputs", "controls", "artifacts_controls.rds"))


