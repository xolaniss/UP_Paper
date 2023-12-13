# Description
# dummies for competition section - UP Team - 2023-11-23
# TO DO:
# The macropru indices will remain as is. The competition indices for entry and exit will now change quite a bit. In the Entry and Exit sheet there are 13 columns that we need to insert based on the buckets above. I've highlighted these in green in the workbook.
# Entry and exit columns for (1)
# Entry and exit columns for (2)
# Entry and exit columns for (3)
# Entry and exit columns for (4) 
# Entry and exit columns for (5)
# Entry and exit columns for (6)
# Entry column for (7)

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
source(here("Functions", "excel_import_sheet.R"))

# Import -------------------------------------------------------------
read_excel(here("Data", 
                "narratives", 
                "Competition_and_inclusion_narrative_index_22112023.xlsx"),
           sheet = 2)
 path <- here("Data", 
              "narratives", 
              "Competition_and_inclusion_narrative_index_22112023.xlsx")

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
         Entry_all = Entry,
         Exit_all = Exit,
         Entry_Corporate = Enter_Corporate) %>% 
  separate(Event, into = c("Event", "Description"), sep = ": ") %>% 
  dplyr::select(-Type, -Year, -Month) %>% 
  relocate(Date, .before = Event)

entry_exit_tbl <- 
  seq(as.Date("2008-01-01"), as.Date("2020-12-31"), by = "month") %>% 
  as_tibble() %>%
  rename(Date = value) %>%
  left_join(entry_exit, by = "Date") %>% 
  replace_na(
    list(
    Entry_all = 0,
    Exit_all = 0,
    Entry_Household = 0,
    Exit_Household = 0,
    Entry_Corporate = 0,
    Exit_Corporate = 0,
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
  seq(as.Date("2008-01-01"), as.Date("2020-12-31"), by = "month") %>% 
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
         Date = `Date Implemented`,
         ) %>% 
  dplyr::select(-Type, -Year, -Month) %>% 
  # floor the dates
  mutate(Date = floor_date(Date, unit = "month"))

# create competition dummy
competition_dummy_tbl <- 
  seq(as.Date("2008-01-01"), as.Date("2020-12-31"), by = "month") %>% 
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
  seq(as.Date("2008-01-01"), as.Date("2020-12-31"), by = "month") %>% 
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
  fx_plot()

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


