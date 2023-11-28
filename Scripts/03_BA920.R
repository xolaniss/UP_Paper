# Description
# Cleaning and importing files - 16 October 2023 - Xolani Sibande
# Preliminaries -----------------------------------------------------------
library(tidyverse)
library(readr)
library(readxl)
library(here)
library(lubridate)
library(xts)
library(tseries)
library(broom)
library(glue)
library(vars)
library(PNWColors)
library(patchwork)
library(psych)
library(kableExtra)
library(strucchange)
library(timetk)
library(pins)
library(uniqtag)
library(scales)
library(urca)
library(mFilter)
library(skimr)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
source(here("Functions", "Sheets_import.R"))
source(here("Functions", "excel_import_sheet.R"))

cleanup <-
  function(data){
    data %>% 
      as_tibble() %>%
      pivot_longer(-...1, names_to = "Date", values_to = "Value") %>% 
      rename("Series" = ...1) %>% 
      mutate(Date = parse_date_time(Date, orders = "mY")) 
      }

# Import -------------------------------------------------------------
path = here("Data", "BA920", "BA920_data.xlsx")
col_types = c("text", rep(x = "numeric", 54))
sheet_list = list(
  "Total_Banks" = "Total Banks")

BA920 <- 
  excel_import_sheet(
  path = path,
  sheet_list = sheet_list,
  skip = 5,
  col_types = col_types 
) 

# Clean up -----------------------------------------------------------------
BA920_clean <- map(BA920, ~cleanup(.))
# EDA ---------------------------------------------------------------------
pivot_eda <- function(data){
  data %>% 
    pivot_wider(values_from = Value, names_from = Series) %>% 
    skim()
}

BA920_eda <- BA920_clean %>% map(~pivot_eda(.))
Total_banks_tbl <- BA920_clean$Total_Banks

# graphing ----------------------------------------------------------------
gg_list <- BA920_clean %>%  map(~fx_plot(., variables_color = 15, ncol = 2))
files <- paste0(here("Outputs"), "/", "BA920/", names(gg_list), ".png")
walk2(gg_list, files,  ~ggsave(., device = "png", file = .y))
write.csv(BA920_clean$Total_Banks, file = here("Outputs", "BA920", "Total_banks.csv"), row.names = F)

# Exporting --------------------------------------------------------
artifacts_BA920 <- list (
  data = list(
    Total_banks_tbl = Total_banks_tbl
  )
)

write_rds(artifacts_BA920, file = here("Outputs", "BA920", "artifacts_BA920.rds"))

