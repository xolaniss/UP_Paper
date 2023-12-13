# Description
# Cleaning up BA900 data and graphing 16 October 2023 - Xolani Sibande
# TO DO: 
# We need to focus the analysis and filter the data
# (1) Credit card, Overdrafts, loans and advances to non-financial sector (168 + 169 + 183 + 185 + 190 + 192)
# (2) Credit card, Overdrafts, loans and advances to non-financial corporate (168 + 183 + 190)
# (3) Credit card, Overdrafts, loans and advances to households (169 + 185 + 192)
# (4) Mortgages to non-financial corporates and households for commercial purposes (152 + 153 + 156 + 163 + 164)
# (5) Residential mortgages to households (157)
# (6) Leasing and installment sales to non-financial corporates (142 + 147)
# (7) Leasing and installment sales to households (143 + 148)

# Packages ----------------------------------------------------------------
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
library(purrr)
library(pins)
library(uniqtag)
library(scales)
library(skimr)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
source(here("Functions", "Sheets_import.R"))
source(here("Functions", "excel_import_sheet.R"))
source(here("Functions", "cleanup.R"))
source(here("Functions", "filter_and_stings.R"))
source(here("Functions", "balance_sheet_rename_gg.R"))
source(here("Functions", "ba900_aggregation.R"))

# Import ------------------------------------------------------------------
path = here("Data", "BA900", "BA900_line_item_103_to_277_updated_to_Aug_2023.xlsx")
sheet_list = list(
  "Total Banks" = "Sheet1", 
  "Absa Bank"  = "Sheet2",
  "FNB" = "Sheet3",
  "Nedbank" = "Sheet5",
  "Standard Bank" = "Sheet6",
  "Capitec" = "Sheet12")
col_types = c("text", rep(x = "numeric", 188)) # Change with update

ba900_assets <- 
  excel_import_sheet(
  path = path,
  sheet_list = sheet_list,
  skip = 5,
  col_types = col_types 
    ) 

ba900_assets %>% glimpse()

# Cleaning ----------------------------------------------------------
total_tbl <- cleanup(ba900_assets$`Total Banks`, date_size = 176) 
absa_tbl <- cleanup(ba900_assets$`Absa Bank`, date_size = 176)
fnb_tbl <- cleanup(ba900_assets$FNB, date_size = 175)
nedbank_tbl <- cleanup(ba900_assets$Nedbank, date_size = 175)
standard_tbl <- cleanup(ba900_assets$`Standard Bank`, date_size = 175)
capitec_tbl <- cleanup(ba900_assets$Capite, date_size = 175)

unique(total_tbl$Series)

# EDA ---------------------------------------------------------------------
total_tbl %>% pivot_wider(names_from = Series, values_from = Value) %>%  skim()
absa_tbl %>% pivot_wider(names_from = Series, values_from = Value) %>%  skim()
fnb_tbl %>% pivot_wider(names_from = Series, values_from = Value) %>%  skim()
nedbank_tbl %>% pivot_wider(names_from = Series, values_from = Value) %>%  skim()
standard_tbl %>% pivot_wider(names_from = Series, values_from = Value) %>%  skim()
capitec_tbl %>% pivot_wider(names_from = Series, values_from = Value) %>%  skim()


#  Filtering --------------------------------------------------------------
# TO DO: We need to decide on which variables to focus on 

# Graphing ---------------------------------------------------------------

## Detail graphs ----
# totals_gg <- 
#   total_tbl %>% 
#   balance_sheet_rename_gg(variable_color = 175)
# absa_gg <-
#   absa_filtered_tbl %>% 
#   balance_sheet_rename_gg(variable_color = 14)
# fnb_gg <- 
#   fnb_filtered_tbl %>% 
#   balance_sheet_rename_gg()
# nedbank_gg <- 
#   nedbank_filtered_tbl %>% 
#   balance_sheet_rename_gg()
# standard_gg <- 
#   standard_filtered_tbl %>% 
#   balance_sheet_rename_gg()
# capitec_gg <- 
#   capitec_filtered_tbl %>% 
#   balance_sheet_rename_gg()


# Export ------------------------------------------------------------------
data_list = list(
  total_tbl = total_tbl,
  absa_tbl = absa_tbl,
  fnb_tbl = fnb_tbl,
  nedbank_tbl = nedbank_tbl,
  standard_tbl = standard_tbl,
  capitec_tbl = capitec_tbl)

files <- paste0(here("Outputs"), "/", "BA900/", names(data_list), ".csv")

walk2(data_list, files, ~ write.csv(x = .x, file = .y, row.names = F )) # exporting top five banks to CSV (as an example)

artifacts_BA900 <- list (
  data = list(
    Total_banks_tbl = Total_banks_tbl
  )
)

write_rds(artifacts_BA900, file = here("Outputs", "BA900", "artifacts_BA900.rds"))









