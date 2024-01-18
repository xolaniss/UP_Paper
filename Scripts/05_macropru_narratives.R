# Description
# macro pru narratives from kea - UP Team - 2023-11-23
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
macropru_narratives <- read_excel(here("Data", "narratives", "plot_narratives_post_2008.xlsx"))

macropru_narratives %>% 
  tail(12)
# Cleaning -----------------------------------------------------------------


macropru_narratives_tbl <- 
  macropru_narratives %>% 
  dplyr::select(-Period) %>%
  mutate(
    Date = seq(as.Date("2008-01-01"), as.Date("2019-12-31"), by = "month")
  ) %>% 
  bind_rows(
    tibble(
      "Date" = seq(as.Date("2020-01-01"), as.Date("2023-12-31"), by = "month"),
      "Draft" = rep(0, 48),
      "Implementation" = rep(0, 48)
    )
  ) %>% 
  # move date to before Draft
  relocate(Date, .before = Draft)

# EDA ---------------------------------------------------------------
macropru_narratives_tbl %>% skim()

# Graphing ---------------------------------------------------------------
macropru_narratives_gg <- 
  macropru_narratives_tbl %>% 
  pivot_longer(
    cols = -Date,
    names_to = "Series",
    values_to = "Value"
  ) %>%
  fx_plot()

# Export ---------------------------------------------------------------
artifacts_macropru_narratives <- list (
  macropru_narratives_tbl = macropru_narratives_tbl,
  macropru_narratives_gg = macropru_narratives_gg
  
)

write_rds(artifacts_macropru_narratives, 
          file = here("Outputs", "narratives", "artifacts_macropru_narratives.rds"))


