# Description
# Calculated weighted rates - Xolani Sibande - 12 Jan 2024
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
lending_rates <- read_rds(here("Outputs", "BA930", "artifacts_BA930.rds"))  
lending <- read_rds(here("Outputs", "BA900", "artifacts_BA900.rds")) 
lending_shares_tbl <- lending$shares_data$combined_shares_tbl %>% 
  pivot_wider(id_cols = c(Date, Banks), names_from = Series, values_from = Value)

# Equal weights approach --------------------------------------------------

## Cleaning lending rates --------------------------------------------------------
lending_rates_tbl <- 
  lending_rates$data$lending_rate_tbl %>% 
  dplyr::select(- `Outstanding balance at month end R'000`, Item) %>% 
  mutate(
    Description = str_replace_all(Description, 
                                  "\"Instalment sale agreements:  flexible rate\": corporate sector", 
                                  "Instalment sale agreements: corporate sector: flexible rate"),
    Description = str_replace_all(Description, 
                                  " fixed rate: corporate sector installment sale agreements", 
                                  "Instalment sale agreements: corporate sector: fixed rate"),
    Description = str_replace_all(Description, 
                                  "\"Instalment sale agreements:  flexible rate\": household sector", 
                                  "Instalment sale agreements: household sector: flexible rate"),
    Description = str_replace_all(Description, 
                                  " fixed rate: household sector installment sale agreements", 
                                  "Instalment sale agreements: household sector: fixed rate"),
    Description = str_replace_all(Description, 
                                  "\"Instalment sale agreements:  flexible rate\": domestic private sector", 
                                  "Instalment sale agreements: domestic private sector: flexible rate"),
    Description = str_replace_all(Description, 
                                  " fixed rate: domestic private sector installment sale agreements", 
                                  "Instalment sale agreements: domestic private sector: fixed rate"),
    Description = str_replace_all(Description, 
                                  "\"Leasing transactions:  flexible rate\": corporate sector", 
                                  "Leasing transactions: corporate sector: flexible rate"),
    Description = str_replace_all(Description, 
                                  "\"Leasing transactions:  flexible rate\": household sector", 
                                  "Leasing transactions: household sector: flexible rate"),
    Description = str_replace_all(Description, 
                                  "\"Leasing transactions:  flexible rate\": domestic private sector", 
                                  "Leasing transactions: domestic private sector: flexible rate"),
    Description = str_replace_all(Description, 
                                  " fixed rate: domestic private sector leasing transactions", 
                                  "Leasing transactions: domestic private sector: fixed rate"),
    Description = str_replace_all(Description, 
                                  " fixed rate: household sector leasing transactions", 
                                  "Leasing transactions: household sector: fixed rate"),
    Description = str_replace_all(Description, 
                                  " fixed rate: corporate sector leasing transactions", 
                                  "Leasing transactions: corporate sector: fixed rate"),
    Description = str_replace_all(Description, 
                                  "\"Mortgage advances:  flexible rate\": corporate sector", 
                                  "Mortgage advances: corporate sector: flexible rate"),
    Description = str_replace_all(Description, 
                                  " fixed rate: corporate sector mortgage advances", 
                                  "Mortgage advances: corporate sector: fixed rate"),
    Description = str_replace_all(Description, 
                                  "\"Mortgage advances:  flexible rate\": household sector", 
                                  "Mortgage advances: household sector: flexible rate"),
    Description = str_replace_all(Description, 
                                  " fixed rate: household sector mortgage advances", 
                                  "Mortgage advances: household sector: fixed rate"),
    Description = str_replace_all(Description, 
                                  "\"Mortgage advances:  flexible rate\": domestic private sector", 
                                  "Mortgage advances: domestic private sector: flexible rate"),
    Description = str_replace_all(Description, 
                                  " fixed rate: domestic private sector mortgage advances", 
                                  "Mortgage advances: domestic private sector: fixed rate"),
    Description = str_replace_all(Description, 
                                  "\"Credit cards:: corporate sector", 
                                  "Credit cards: corporate sector"),
    Description = str_replace_all(Description, 
                                  "\"Credit cards: \": household sector", 
                                  "Credit cards: household sector"),
    Description = str_replace_all(Description, 
                                  "\"Credit cards: \": domestic private sector", 
                                  "Credit cards: domestic private sector"),
    ) %>% 
  pivot_wider(id_cols = c(Date, Banks), 
            names_from = Description,
            values_from = `Weighted average rate (%)`) %>% 
  dplyr::select(-ends_with("fixed rate"), 
                -contains("domestic private sector"),
                -contains("foreign sector"),
                -contains("Micro loans"),
                -contains("Interbank"))
names(lending_rates_tbl) 

## Graphing lending rates ------------------------------------------------
lending_rate_gg <-
  lending_rates_tbl %>%
  pivot_longer(cols = -c(Date, Banks), names_to = "Series", values_to = "Value") %>%
  filter(Banks == "Total Banks") %>%
  dplyr::select(Date, Series, Value) %>%
  fx_plot(plotname = "Rate (%)", variables_color = 12) +
  facet_wrap(~ Series, scales = "free_y", ncol = 2)

lending_rate_gg

## Aggregated rates -----------------------------------------------------------
aggregated_lending_rates_tbl <-
  lending_rates_tbl %>% 
  left_join(lending_shares_tbl, by = c("Date", "Banks")) %>%
  mutate(
    `Non financial corporate unsecured lending rate` =
        `Credit cards: corporate sector` * `Non-financial corporate sector credit cards share` +
          `Overdrafts: corporate sector` * `Non-financial corporate sector overdrafts share`  +
          `Other: corporate sector` * `Non-financial corporate sector other loans and advances share`
  ) %>%
  mutate(
    `Household unsecured lending rate` =
        `Credit cards: household sector` * `Households credit cards share` +
          `Overdrafts: household sector` * `Households overdrafts share`  +
          `Other: household sector` * `Households other loans and advances share`
  )  %>%
  mutate(
    `Total unsecured lending rate` =
        `Non financial corporate unsecured lending rate` * `Non-financial corporate unsecured lending share` +
          `Household unsecured lending rate` * `Household unsecured lending share`
  ) %>% 
  mutate(
    `Commercial mortgages to corporates and households rate` =
        `Mortgage advances: corporate sector: flexible rate` 
      ) %>% 
  mutate(
    `Residential mortgages to household rate` =
        `Mortgage advances: household sector: flexible rate` 
      ) %>% 
  mutate(
    `Total mortgages lending rate` =
      `Commercial mortgages to corporates and households rate` *
      `Commercial mortgages to corporates and households share`  +
        `Residential mortgages to household rate` *
        `Residential mortgages to households share`
  ) %>% 
  mutate(
    `Leasing and installements to corporate rate` =
        `Leasing transactions: corporate sector: flexible rate` *
          `Non-financial corporate sector leasing transactions share` +
          `Instalment sale agreements: corporate sector: flexible rate` *
          `Non-financial corporate sector instalment sales share` 
  ) %>%
  mutate(
      `Leasing and installments to households rate` =
          `Leasing transactions: household sector: flexible rate` *
            `Households leasing transactions share`+
            `Instalment sale agreements: household sector: flexible rate` *
            `Household sector instalment sales share`
    ) %>% 
  mutate(
    `Total leasing and installments rate` =
        `Leasing and installements to corporate rate` *
          `Leasing and installments to corporates share`+
          `Leasing and installments to households rate` *
          `Leasing and installments to households share`
  ) %>%
  dplyr::select(
    Date,
    Banks,
    `Non financial corporate unsecured lending rate`,
    `Household unsecured lending rate`,
    `Total unsecured lending rate`,
    `Commercial mortgages to corporates and households rate`,
    `Residential mortgages to household rate`,
    `Total mortgages lending rate`,
    `Leasing and installements to corporate rate`,
    `Leasing and installments to households rate`,
    `Total leasing and installments rate`
  ) 
  
aggregated_lending_rates_tbl %>% 
  

aggregated_lending_rates_gg <- 
  aggregated_lending_rates_tbl %>% 
  pivot_longer(cols = -c(Date, Banks), names_to = "Series", values_to = "Value") %>%
  filter(Banks == "Total Banks") %>%
  dplyr::select(Date, Series, Value) %>%
  fx_plot(plotname = "Rate (%)", variables_color = 10) +
  facet_wrap(~ Series, scales = "free_y", ncol = 2)
  

# Export ---------------------------------------------------------------
artifacts_lending_rates_futher_analysis <- 
  list (
    data = list(
      lending_rates_tbl = lending_rates_tbl,
      aggregated_lending_rates_tbl = aggregated_lending_rates_tbl
    ),
    gg = list(
      lending_rate_gg = lending_rate_gg,
      aggregated_lending_rates_gg = aggregated_lending_rates_gg
    )
)

write_rds(artifacts_lending_rates_futher_analysis, 
          file = here("Outputs", "BA930", "artifacts_BA930_futher_analysis.rds"))


