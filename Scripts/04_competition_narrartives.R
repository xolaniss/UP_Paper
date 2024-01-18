# Description
# dummies for competition section - UP Team - 2023-11-23

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
source(here("Functions", "excel_import_sheet.R"))

# Import -------------------------------------------------------------
read_excel(here("Data", 
                "Narratives", 
                "Competition_and_inclusion_narrative_index_10122023.xlsx"),
           sheet = 2)
 path <- here("Data", 
              "Narratives", 
              "Competition_and_inclusion_narrative_index_10122023.xlsx")

sheet_list <- list(
   "Entry and Exit" = "Entry and Exit",
   "Competition" = "Competition",
   "Finance regulations" = "Finance regulations",
   "Financial inclusion" = "Financial inclusion"
 )
 
competition_narratives <- 
  path %>% 
  excel_import_sheet(sheet_list = sheet_list)

# Entry exit dummy-----------------------------------------------------------------
entry_exit <- 
  competition_narratives[[1]] %>% 
  rename(Event = `Policy / Initiative / Development`,
         Date = `Date Implemented`,
         Entry_all_dummy = Entry,
         Exit_all_dummy = Exit,
         Entry_Household_dummy = Entry_Household,
         Exit_Household_dummy = Exit_Household,
         Entry_Corporate_dummy = Enter_Corporate,
         Exit_Corporate_dummy = Exit_Corporate,
         Entry_credit_non_fin_dummy = `Entry - Credit card, Overdrafts, loans and advances to non-financial sector`,         
         Exit_credit_non_fin_dummy = `Exit - Credit card, Overdrafts, loans and advances to non-financial sector`,          
         Entry_credit_non_fin_corporate_dummy = `Entry - Credit card, Overdrafts, loans and advances to non-financial corporate`,      
         Exit_credit_non_fin_corporate_dummy = `Exit - Credit card, Overdrafts, loans and advances to non-financial corporate`,       
         Entry_credit_households_dummy = `Entry - Credit card, Overdrafts, loans and advances to households`,                   
         Exit_credit_households_dummy = `Exit - Credit card, Overdrafts, loans and advances to households`,                    
         Entry_commercial_mortgages_dummy = `Entry - Mortgages to non-financial corporates and households for commercial purposes`,
         Exit_commercial_mortgages_dummy = `Exit - Mortgages to non-financial corporates and households for commercial purposes`, 
         Entry_mortgages_households_dummy = `Entry - Residential mortgages to households`,                                         
         Exit_mortgages_households_dummy = `Exit - Residential mortgages to households`,                                          
         Entry_leasing_households_dummy = `Entry - Household leasing or installment`,                                            
         Exit_leasing_households_dummy = `Exit - Household leasing or installment`,                                             
         Entry_leasing_non_fin_corporate_dummy = `Entry - Non-financial corporate leasing or installment`
         ) %>% 
  separate(Event, into = c("Event", "Description"), sep = ": ") %>% 
  dplyr::select(-Type, -Year, -Month) %>% 
  relocate(Date, .before = Event) 


entry_exit_tbl <- 
  seq(as.Date("2008-01-01"), as.Date("2023-12-31"), by = "month") %>% 
  as_tibble() %>%
  rename(Date = value) %>%
  left_join(entry_exit, by = "Date") %>% 
  replace_na(
    list(
    Entry_all_dummy = 0,
    Exit_all_dummy = 0,
    Entry_Household_dummy = 0,
    Exit_Household_dummy = 0,
    Entry_Corporate_dummy = 0,
    Exit_Corporate_dummy = 0,
    Entry_Corporate_dummy = 0,
    Entry_credit_non_fin_dummy = 0,
    Exit_credit_non_fin_dummy = 0,
    Entry_credit_non_fin_corporate_dummy = 0,
    Exit_credit_non_fin_corporate_dummy = 0,
    Entry_credit_households_dummy = 0,
    Exit_credit_households_dummy = 0,
    Entry_commercial_mortgages_dummy = 0,
    Exit_commercial_mortgages_dummy = 0,
    Entry_mortgages_households_dummy = 0,
    Exit_mortgages_households_dummy = 0,
    Entry_leasing_households_dummy = 0,
    Exit_leasing_households_dummy = 0,
    Entry_leasing_non_fin_corporate_dummy = 0,
    Event = "No event",
    Description = "No event"
    )
  )

# Finance regulations dummy --------------------------------------------------------
finance_regulations_tbl <- 
  competition_narratives[[3]] %>% 
  rename(Event = `Policy / Initiative / Development`,
         Date = `Date Implemented`,
         ) %>% 
  dplyr::select(-Type, -Year, -Month) %>% 
  # floor the dates
  mutate(Date = floor_date(Date, unit = "month")) %>% 
  # delete duplicate events
  distinct(Date, Event, .keep_all = TRUE)

finance_regulation_dummuy_tbl <- 
  seq(as.Date("2008-01-01"), as.Date("2023-12-31"), by = "month") %>% 
  as_tibble() %>% 
  rename(Date = value) %>% 
  mutate(event_dummy = ifelse(Date %in% finance_regulations_tbl$Date, 1, 0)) %>% 
  # rename event_dummy to finance_regulation_dummy
  rename(finance_regulation_dummy = event_dummy)

finance_regulation_dummuy_tbl %>% skim()

# Competition dummy ---------------------------------------------------------------
competition_tbl <- 
  competition_narratives[[2]] %>% 
  as_tibble() %>%
  rename(Event = `Policy / Initiative / Development`,
         Date = Year
         ) %>%
  mutate(Date = paste0(Date, "-01-01")) %>% # may need to change given two similar dates
  dplyr::select(-Type, -`Date Implemented`, -Month) %>% 
  # floor the dates
  mutate(Date = floor_date(as.Date(Date), unit = "month"))

# create competition dummy
competition_dummy_tbl <- 
  seq(as.Date("2008-01-01"), as.Date("2023-12-31"), by = "month") %>% 
  as_tibble() %>% 
  rename(Date = value) %>% 
  mutate(event_dummy = ifelse(Date %in% competition_tbl$Date, 1, 0)) %>% 
  # rename event_dummy to competition_dummy
  rename(competition_dummy = event_dummy)

competition_dummy_tbl %>% skim()

# Financial inclusion dummy ---------------------------------------------------------------
financial_inclusion_tbl <- 
  competition_narratives[[4]] %>% 
  as_tibble() %>%
  rename(Event = `Policy / Initiative / Development`,
         Date = `Date Implemented`,
         ) %>% 
  dplyr::select(-Type, -Year, -Month) %>% 
  # floor the dates
  mutate(Date = floor_date(Date, unit = "month"))

# create financial inclusion dummy
financial_inclusion_dummy_tbl <- 
  seq(as.Date("2008-01-01"), as.Date("2023-12-31"), by = "month") %>% 
  as_tibble() %>% 
  rename(Date = value) %>% 
  mutate(event_dummy = ifelse(Date %in% financial_inclusion_tbl$Date, 1, 0)) %>% 
  # rename event_dummy to financial_inclusion_dummy
  rename(financial_inclusion_dummy = event_dummy)
  
# combining dummies -------------------------------------------------------
# join finance_regulations_tbl and competition_tbl
combined_dummies_tbl <- 
  entry_exit_tbl %>% 
  left_join(finance_regulation_dummuy_tbl, by = "Date") %>%
  left_join(competition_dummy_tbl, by = "Date") %>% 
  left_join(financial_inclusion_dummy_tbl, by = "Date")

combined_dummies_gg <- 
  combined_dummies_tbl %>%
  pivot_longer(c(-Date, -Event, -Description), names_to = "Series", values_to = "Value") %>%
  fx_plot(variables_color = 22) +
  facet_wrap(~Series, scales = "free_y", ncol = 3)

combined_dummies_gg
# Export ---------------------------------------------------------------
artifacts_competion_narratives <- list (
    data = list(
      entry_exit_tbl = entry_exit_tbl,
      finance_regulation_dummuy_tbl = finance_regulation_dummuy_tbl,
      competition_dummy_tbl = competition_dummy_tbl,
      financial_inclusion_dummy_tbl = financial_inclusion_dummy_tbl,
      combined_dummies_tbl = combined_dummies_tbl
    ),
    graphs = list(
      combined_dummies_gg = combined_dummies_gg
    )
)

write_rds(artifacts_competion_narratives, 
          file = here("Outputs", "narratives", "artifacts_competion_narratives.rds"))


