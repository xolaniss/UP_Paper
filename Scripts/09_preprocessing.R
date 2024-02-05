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
library(datawizard)

# econometrics
library(tseries)
library(strucchange)
library(vars)
library(urca)
library(mFilter)
library(car)
library(lpirfs)
options(scipen = 999)
# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))

# Import -------------------------------------------------------------
combined <- read_rds(here("Outputs", "combined_data", "artifacts_data_check.rds"))
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
controls_tbl <- controls$controls_tbl %>% 
  mutate(Banks = str_to_title(Bank)) %>% 
  dplyr::select(-Bank) %>%
  relocate(Banks, .after = Date) %>% 
  pivot_wider(id_cols = c(Date, Banks), names_from = "Series", values_from = "Value")
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

## Combining lending and lending rates and dummies and controls -------------------------------------------------------------
combined_tbl <- 
  lending_tbl %>% 
  left_join(dummies_tbl, by = c("Date", "Banks")) %>% # combined lending and dummies
  left_join(controls_tbl, by = c("Date", "Banks"))   # combined lending and dummies and controls

glimpse(combined_tbl)


# EDA ---------------------------------------------------------------
combined_tbl %>% group_by(Banks) %>% skim()
plot_str(combined_tbl) # structure of data
plot_intro(combined_tbl) # breakdown on data
plot_missing(combined_tbl) # breakdown on missing
plot_bar(combined_tbl) # freq of discrete
plot_histogram(combined_tbl) # distribution of continuous
plot_boxplot(combined_tbl, by = "Banks") # distribution of continuous
plot_qq(combined_tbl) # qq of continuous
#

# Feature engineering --------------------------------------------------------
## logs

combined_features_tbl <- 
  combined_tbl %>% 
  filter(Banks != "Capitec") %>% # because of "missing" or zero values
  mutate(across(c(`Total unsecured lending`,
                  `Non-financial corporate unsecured lending`,
                  `Household unsecured lending`,
                  `Total mortgage lending`,
                  `Commercial mortgages to corporates and households`,
                  `Residential mortgages to households`,
                  `Total leasing and installments`,
                  `Leasing and installments to corporates`,
                  `Leasing and installments to households`
                  ), 
         ~log(.x + 1), 
         .names = "log_{.col}"
          )) %>%  # log transformation 
  mutate(across(c(`log_Total unsecured lending`,
                  `log_Non-financial corporate unsecured lending`,
                  `log_Household unsecured lending`,
                  `log_Total mortgage lending`,
                  `log_Commercial mortgages to corporates and households`,
                  `log_Residential mortgages to households`,
                  `log_Total leasing and installments`,
                  `log_Leasing and installments to corporates`,
                  `log_Leasing and installments to households`
                  ), 
         ~ (.x - lag(.x)),
         .names = "Change_in_{.col}"
          )) %>%  # log transformation
  mutate(across(c(
    `Change_in_log_Total unsecured lending`,
    `Change_in_log_Non-financial corporate unsecured lending`,
    `Change_in_log_Household unsecured lending`,
    `Change_in_log_Total mortgage lending`,
    `Change_in_log_Commercial mortgages to corporates and households`,
    `Change_in_log_Residential mortgages to households`,
    `Change_in_log_Total leasing and installments`,
    `Change_in_log_Leasing and installments to corporates`,
    `Change_in_log_Leasing and installments to households`
  ),
  ~ .x * 100)) %>%  # multiply by 100 to get percentage change
  mutate(across(c(
    `Non financial corporate unsecured lending rate`,
    `Household unsecured lending rate`,
    `Total mortgages lending rate`,
    `Commercial mortgages to corporates and households rate`,
    `Residential mortgages to household rate`,
    `Total leasing and installments rate`,
    `Leasing and installements to corporate rate`,
    `Leasing and installments to households rate`,
    `Total unsecured lending rate`
  ), 
                ~ winsorize(.x, threshold = .01))) %>% # winsorize rates
  mutate(across(
    c(
      `Total assets`,
      `Gross loan advances`,
      `Retained earnings`,
      `Level one high-quality liquid assets required to be held`,
      `Average daily amount of level one high-quality liquid assets`,
      `Aggregate risk weighted exposure`
      ),
    ~ log(.x + 1),
    .names = "log_{.col}"
    )) # log transformation of controls

combined_features_tbl %>% glimpse()

plot_histogram(combined_features_tbl, ncol = 2)
plot_boxplot(combined_features_tbl %>%  dplyr::select(Banks, contains("Change_in")),
             by = "Banks")
plot_boxplot(combined_features_tbl %>%  dplyr::select(Banks, contains("rate")),
             by = "Banks")
# Export ---------------------------------------------------------------
artifacts_modelling_data <- list (
  combined_tbl = combined_tbl,
  combined_features_tbl = combined_features_tbl
)

write_rds(artifacts_modelling_data, file = here("Outputs", "combined_data", "artifacts_modelling_data.rds"))


