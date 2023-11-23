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
entry_exit_tbl <- 
  competition_narratives[[1]] %>% 
  rename(Event = `Policy / Initiative / Development`,
         Date = `Date Implemented`,
         Entry_all = Entry,
         Exit_all = Exit,
         Entry_Corporate = Enter_Corporate) %>% 
  separate(Event, into = c("Event", "Description"), sep = ": ") %>% 
  dplyr::select(-Type, -Year, -Month) %>%
  relocate(Date, .before = Event) %>% 
  replace_na(list(Entry_Corporate = 0))


entry_exit_tbl
entry_exit_tbl %>% skim()


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
  mutate(event_dummy = ifelse(Date %in% finance_regulations_tbl$Date, 1, 0)) 

finance_regulation_dummuy_tbl %>% skim()
finance_regulation_dummuy_tbl %>% 
  # create dummy plot
  ggplot(aes(x = Date, y = event_dummy)) +
  geom_line()

# Competition dummy ---------------------------------------------------------------
competition_tbl <- 
  competition_narratives[[2]] %>% 
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
  mutate(event_dummy = ifelse(Date %in% competition_tbl$Date, 1, 0))

competition_dummy_tbl %>% skim()

# create dummy plot
competition_dummy_tbl %>% 
  ggplot(aes(x = Date, y = event_dummy)) +
  geom_line()

# Financial inclusion dummy ---------------------------------------------------------------
financial_inclusion_tbl <- 
  competition_narratives[[4]] %>% 
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
  mutate(event_dummy = ifelse(Date %in% financial_inclusion_tbl$Date, 1, 0))
  
# plot dummy
financial_inclusion_dummy_tbl %>% 
  ggplot(aes(x = Date, y = event_dummy)) +
  geom_line()

# Export ---------------------------------------------------------------
artifacts_competion_narratives <- list (
    data = list(
      entry_exit_tbl = entry_exit_tbl,
      finance_regulation_dummuy_tbl = finance_regulation_dummuy_tbl,
      competition_dummy_tbl = competition_dummy_tbl,
      financial_inclusion_dummy_tbl = financial_inclusion_dummy_tbl
    ),
    graphs = list(
      financial_inclusion_dummy_tbl = financial_inclusion_dummy_tbl
    )
)

write_rds(artifacts_competion_narratives, 
          file = here("Outputs", "narratives", "artifacts_competion_narratives.rds"))


