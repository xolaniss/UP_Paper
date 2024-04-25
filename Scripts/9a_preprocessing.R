# Description
# preprocessing for interest spread - Xolani Sibande - 2024-04-01

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
combined_tbl <- combined$combined_features_tbl

combined_tbl %>% glimpse()


# Calculating spreads  ------------------------------------------------------
combined_spread_tbl <- 
  combined_tbl %>% 
  mutate(
    `Non financial corporate unsecured lending spread` = 
      `Non financial corporate unsecured lending rate` - repo,
    `Household unsecured lending spread` = 
      `Household unsecured lending rate`- repo,
    `Total unsecured lending spread` = 
      `Total unsecured lending rate` - repo ,
    `Commercial mortgages to corporates and households spread` = 
      `Commercial mortgages to corporates and households rate` - repo ,
    `Residential mortgages to household spread` = 
      `Residential mortgages to household rate` - repo,
    `Total mortgages lending spread` = 
      `Total mortgages lending rate` - repo,
    `Leasing and installements to corporate spread` = 
      `Leasing and installements to corporate rate` - repo,
    `Leasing and installments to households spread` = 
      `Leasing and installments to households rate` - repo,
    `Total leasing and installments spread` = 
      `Total leasing and installments rate` - repo
  )

combined_spread_tbl %>% glimpse()

# graphing -----------------------------------------------------------------
combined_spread_gg <- 
  combined_spread_tbl %>% 
  dplyr::select(Date, Banks, ends_with("spread")) %>%
  filter(Banks != "Capitec", Banks != "Total Banks") %>% 
  pivot_longer(cols = -c(Date, Banks), names_to = "Series", values_to = "Value") %>%
  group_by(Banks) %>% 
  ggplot(aes(x = Date, y = Value, color = Series)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~Banks, scales = "free_y")

combined_spread_gg

# Export ---------------------------------------------------------------
artifacts_combined_spreads <- list (
    combined_spread_tbl = combined_spread_tbl,
    combined_spread_gg = combined_spread_gg
)

write_rds(artifacts_combined_spreads, file = here("Outputs","combined_data", "artifacts_combined_spreads.rds"))


