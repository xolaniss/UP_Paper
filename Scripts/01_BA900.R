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

## Filter vec -----------------------------------------------------------
filter_vec <- 
  c(  
    "DEPOSITS, LOANS AND ADVANCES (total of items 111, 117, 118, 126, 135, 139, 150, 166, 171 and 180, less item 194)"
    # , "SA banksb (total of items 112 and 116)"                                                                                                                      
    # , "NCDs/PNsc issued by banks, with an unexpired maturity of: (total of items 113 to 115)"                                                                       
    # , "Up to 1 month"                                                                                                                                               
    # , "More than 1 month to 6 months"                                                                                                                               
    # , "More than 6 months"                                                                                                                                          
    # , "Other deposits with and loans and advances to SA banksb"                                                                                                     
    # , "Deposits with and loans and advances to foreign banks, denominated in rand"                                                                                  
    # , "Loans granted under resale agreements to:  (total of items 119 to 125)"                                                                                      
    # , "SA Reserve Bank"                                                                                                                                             
    # , "Banksd"                                                                                                                                                      
    # , "Insurers"                                                                                                                                                    
    # , "Pension funds"                                                                                                                                               
    # , "Other financial corporate sectorb"                                                                                                                           
    # , "Non-financial corporate sector"                                                                                                                              
    # , "Other"                                                                                                                                                       
    # , "Foreign currency loans and advances (total of items 127 to 130, 133 and 134)"                                                                                
    # , "Foreign currency notes and coin"                                                                                                                             
    # , "Deposits with and advances to SA Reserve Bank"                                                                                                               
    # , "Deposits with and advances to SA banksd"                                                                                                                     
    # , "Other advances to: (total of items 131 and 132)"                                                                                                             
    # , "SA Financial corporate sectorc"                                                                                                                              
    # , "SA Non-financial corporate sector and other"                                                                                                                 
    # , "Deposits with and advances to foreign banks"                                                                                                                 
    # , "Other advances to foreign sector"                                                                                                                            
    # , "Redeemable preference shares issued by: (total items 136 to 138)"                                                                                            
    # , "Banksd-1"                                                                                                                                                    
    # , "Financial corporate sectorc"                                                                                                                                 
    # , "Non-financial corporate sector and other"                                                                                                                    
    # , "Instalment debtors, suspensive sales and leases (total of items 140 and 145)"                                                                                
    # , "Instalment sales (total of items 141 to 144)"                                                                                                                
    # , "Financial corporate sector"                                                                                                                                  
    , "Non-financial corporate sector-1"
    , "Household sector"
    # , "Otherb"                                                                                                                                                      
    # , "Leasing transactions (total of items 146 to 149)"                                                                                                            
    # , "Financial corporate sector-1"                                                                                                                                
    , "Non-financial corporate sector-2"
    , "Household sector-1"
    # , "Otherb-1"                                                                                                                                                    
    # "Mortgage advances (total of items 151, 155 and 159)"                                                                                                         
    # "Farm mortgages: (total of items 152 to 154)"                                                                                                                 
    , "Corporate sector"
    , "Household sector-2"
    , "Otherb-2"
    # ,"Residential mortgages: (total of items 156 to 158)"                                                                                                          
    , "Corporate sector-1"
    , "Household sector-3"
    , "Otherb-3"
    # , "Commercial and other mortgage advances: (total of items 160 to 165)"
    # , "Public financial corporate sector"                                                                                                                           
    # , "Public non-financial corporate sector"                                                                                                                       
    # , "Private financial corporate sector"                                                                                                                          
    , "Private non-financial corporate sector"
    , "Household sector-4"
    , "Otherb-4"
    # , "Credit-card debtors (total of items 167 to 170)"                                                                                                             
    # , "Financial corporate sector-2"                                                                                                                                
    , "Non-financial corporate sector-3"
    , "Household sector-5"
    # , "Otherb-5"                                                                                                                                                    
    # , "Overdrafts, loans and advances: public sector (total of items 172 to 179)"                                                                                   
    # , "Central government of the Republic (excluding social security funds)"                                                                                        
    # , "Social security funds"                                                                                                                                       
    # , "Provincial governments"                                                                                                                                      
    # , "Local government"                                                                                                                                            
    # , "Land Bank"                                                                                                                                                   
    # , "Other public financial corporate sector (such as IDC)c"                                                                                                      
    # , "Public non-financial corporate sector (such as Transnet, Eskom and Telkom)"                                                                                  
    # , "Foreign public sector"                                                                                                                                       
    # , "Overdrafts, loans and advances: private sector (total of items 181, 187 and 188)"                                                                            
    # , "Overdrafts, including overdrafts under cash-management schemes: (total of items 182 to 186)"                                                                 
    # , "Financial corporate sector-3"                                                                                                                                
    , "Non-financial corporate sector-4"
    , "Unincorporated business enterprises of households"
    , "Households"
    , "Non-profit organisations serving households"
    , "Factoring debtors"
    # , "Other loans and advances: (total of items 189 to 193)"                                                                                                       
    # , "Financial corporate sector-4"                                                                                                                                
    , "Non-financial corporate sector-5"
    , "Unincorporated business enterprises of households-1"
    , "Households-1"
    , "Non-profit organisations serving households-1"
    # , "Less: credit impairments in respect of loans and advances" 
  )

## Filtering and graphs -------------------------------------------------------------
total_filtered_tbl <- filter_and_strings(total_tbl, filter_vec) %>%  balance_sheet_rename() 
total_filtered_tbl 
absa_filtered_tbl <- filter_and_strings(absa_tbl, filter_vec) %>%  balance_sheet_rename()
absa_filtered_tbl
fnb_filtered_tbl <- filter_and_strings(fnb_tbl, filter_vec) %>%  balance_sheet_rename()
fnb_filtered_tbl
nedbank_filtered_tbl <- filter_and_strings(nedbank_tbl, filter_vec) %>%  balance_sheet_rename()
nedbank_filtered_tbl
standard_filtered_tbl <- filter_and_strings(standard_tbl, filter_vec) %>%  balance_sheet_rename()
standard_filtered_tbl
capitec_filtered_tbl <- filter_and_strings(capitec_tbl, filter_vec) %>%  balance_sheet_rename()
capitec_filtered_tbl

totals_gg <-
  total_filtered_tbl %>%
  balance_sheet_rename_gg(variable_color = 25)
absa_gg <-
  absa_filtered_tbl %>%
  balance_sheet_rename_gg(variable_color = 25)
fnb_gg <-
  fnb_filtered_tbl %>%
  balance_sheet_rename_gg(variable_color = 25)
nedbank_gg <-
  nedbank_filtered_tbl %>%
  balance_sheet_rename_gg(variable_color = 25)
standard_gg <-
  standard_filtered_tbl %>%
  balance_sheet_rename_gg(variable_color = 25)
capitec_gg <-
  capitec_filtered_tbl %>%
  balance_sheet_rename_gg(variable_color = 25)

unique(total_filtered_tbl$Series)


## Aggregation and graphs -----------------------------------------------------------
total_aggregation_tbl <- ba900_aggregration(total_filtered_tbl)
total_aggregation_gg <- 
  total_aggregation_tbl %>% 
  balance_sheet_rename_gg(variable_color = 9)

absa_aggregation_tbl <- ba900_aggregration(absa_filtered_tbl)
absa_aggregation_tbl <- ba900_aggregration(absa_filtered_tbl)
absa_aggregation_gg <- 
  absa_aggregation_tbl %>% 
  balance_sheet_rename_gg(variable_color = 9)

fnb_aggregation_tbl <- ba900_aggregration(fnb_filtered_tbl)
fnb_aggregation_gg <- 
  fnb_aggregation_tbl %>% 
  balance_sheet_rename_gg(variable_color = 9)

nedbank_aggregation_tbl <- ba900_aggregration(nedbank_filtered_tbl)
nedbank_aggregation_gg <- 
  nedbank_aggregation_tbl %>% 
  balance_sheet_rename_gg(variable_color = 9)

standard_aggregation_tbl <- ba900_aggregration(standard_filtered_tbl)
standard_aggregation_gg <- 
  standard_aggregation_tbl %>% 
  balance_sheet_rename_gg(variable_color = 9)

capitec_aggregation_tbl <- ba900_aggregration(capitec_filtered_tbl)
capitec_aggregation_gg <- 
  capitec_aggregation_tbl %>% 
  balance_sheet_rename_gg(variable_color = 10)

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
  filtered_data = list(
    total_filtered_tbl = total_filtered_tbl,
    absa_filtered_tbl = absa_filtered_tbl,
    fnb_filtered_tbl = fnb_filtered_tbl,
    nedbank_filtered_tbl = nedbank_filtered_tbl,
    standard_filtered_tbl = standard_filtered_tbl,
    capitec_filtered_tbl = capitec_filtered_tbl
  ),
  aggregated_data = list(
    total_aggregation_tbl = total_aggregation_tbl,
    absa_aggregation_tbl = absa_aggregation_tbl,
    fnb_aggregation_tbl = fnb_aggregation_tbl,
    nedbank_aggregation_tbl = nedbank_aggregation_tbl,
    standard_aggregation_tbl = standard_aggregation_tbl,
    capitec_aggregation_tbl = capitec_aggregation_tbl
  ),
  filtered_graphs = list(
    totals_gg = totals_gg,
    absa_gg = absa_gg,
    fnb_gg = fnb_gg,
    nedbank_gg = nedbank_gg,
    standard_gg = standard_gg,
    capitec_gg = capitec_gg
  ),
  aggregated_graphs = list(
    total_aggregation_gg = total_aggregation_gg,
    absa_aggregation_gg = absa_aggregation_gg,
    fnb_aggregation_gg = fnb_aggregation_gg,
    nedbank_aggregation_gg = nedbank_aggregation_gg,
    standard_aggregation_gg = standard_aggregation_gg,
    capitec_aggregation_gg = capitec_aggregation_gg
  )
)

write_rds(artifacts_BA900, file = here("Outputs", "BA900", "artifacts_BA900.rds"))









