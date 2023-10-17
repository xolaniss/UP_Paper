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

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
source(here("Functions", "Sheets_import.R"))
source(here("Functions", "excel_import_sheet.R"))
# Import -------------------------------------------------------------
sheet_list = list(
  "Total Banks" = "Total Banks", 
  "Absa Bank"  = "ABSA",
  "FNB" = "First_Rand",
  "Nedbank" = "Nedbank",
  "Standard Bank" = "Standard_Bank",
  "Capitec" = "Capitec")

year <- seq(2008, 2022, by = 1)
path_list <- 
  year %>% 
  map(~ glue(
  here("Data","BA930", "4.1 BA930 Multiple Bank Export ({.}).xlsx")
))

lending_rate <- 
  path_list %>% 
  purrr::set_names(year) %>% 
  map(~excel_import_sheet(
    path = .,
    sheet_list = sheet_list,
    skip = 5,
    col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric") 
  )) 

# Cleaning -----------------------------------------------------------------
names <- c("Banks", 
           "Description", 
           "Date",  
           "Item", 
           "Outstanding balance at month end R'000",
           "Weighted average rate (%)",
           "...6",
           "...7")
lending_rate_no_names <- 
  lending_rate %>%
  map(~bind_rows(., .id = "Banks")) %>% 
  map(~setNames(., names)) %>% 
  map(~ filter(., Item > 46)) %>% 
  map(~dplyr::select(., 1:6))

lending_rate_tbl <- 
  lending_rate_no_names %>% 
  bind_rows() %>% 
  relocate(Date,.before = Banks) %>% 
  mutate(Date = parse_date_time(Date, "b-Y")) %>% 
  mutate(Description = na.locf(Description)) %>%
  mutate(`Item Description` = dplyr::case_when(
        Item == 47 | Item == 48 | Item == 49 | Item == 51 | Item == 53 | Item ==55 | Item == 56  ~ "corporate sector",
        Item == 50 ~ "corporate sector installment sale agreements",
        Item == 52 ~ "corporate sector leasing transactions",
        Item == 54 ~ "corporate sector mortgage advances", 
        Item == 57 | Item == 58 | Item == 59 |  Item == 61 | Item == 63 | Item ==65 | Item ==66 ~  "household sector",
        Item == 60 ~ "household sector installment sale agreements",
        Item == 62 ~ "household sector leasing transactions",
        Item == 64 ~ "household sector mortgage advances",
        Item == 67 | Item == 68 | Item == 69 | 
        Item == 70 | Item == 71 | Item == 72 | 
        Item == 73 ~ "foreign sector",
        Item == 74 | Item ==75 | Item == 76 | Item ==78 |  Item ==80 | Item == 82 | Item == 83  ~ "domestic private sector", 
        Item == 77 ~ "domestic private sector installment sale agreements",
        Item == 79 ~ "domestic private sector leasing transactions",
        Item == 81 ~ "domestic private sector mortgage advances",
        Item == 84 ~ "Micro loans",
        Item == 85 ~ "Interbank lending rate",
        Item == 86 ~ "Hash total"
  )) %>%
  relocate('Item Description', .after = `Item Description`) %>% 
  mutate(Description = str_c(Description, "-", `Item Description`))

unique(lending_rate_tbl$`Item Description`) #check

# Graphing ---------------------------------------------------------------
lending_rate_gg <- function(bank = "Total Banks"){
  lending_rate_pivot_tbl <- 
    lending_rate_tbl %>% 
    mutate(Description = str_c(Banks, " ", Description)) %>% 
    dplyr::select(-Banks, -Item, -`Outstanding balance at month end R'000`) %>% 
    rename(
      "Series" = "Description",
      "Value" = "Weighted average rate (%)") %>% 
    drop_na() %>% 
    filter(str_detect(Series, bank)) %>% 
    mutate(Value = Value/100) %>% 
    mutate(Series = str_to_sentence(Series)) %>% 
    mutate(Series = str_remove_all(Series, "\"")) %>% 
    mutate(Series = str_replace_all(Series, ":", "-")) %>% 
    mutate(Series = str_replace_all(Series, "- -", "-"))
  
  lending_rate_pivot_tbl %>% 
    fx_nopivot_plot(variables_color = 35, ncol = 3) +
    theme(
      text = element_text(size = 5),
      strip.background = element_rect(colour = "white", fill = "white"),
      axis.text.x = element_text(angle = 90),
      axis.title = element_text(size = 5),
      plot.tag = element_text(size = 5)
    ) +
    scale_y_continuous(labels = scales::label_percent()) 
}

Total_banks_gg <- lending_rate_gg("Total Banks")
Absa_gg <- lending_rate_gg("Absa")  
Standard_gg <- lending_rate_gg("Standard")
FNB_gg <- lending_rate_gg("FNB")
Nedbank_gg <- lending_rate_gg("Nedbank")
Capitec_gg <- lending_rate_gg("Capitec")

# Export ---------------------------------------------------------------
artifacts_BA930 <- list (
  data = list(
    lending_rate_tbl = lending_rate_tbl
  ),
  graphs = list(
    Total_banks_gg = Total_banks_gg,
    Absa_gg = Absa_gg,
    Standard_gg = Standard_gg,
    FNB_gg = FNB_gg,
    Nedbank_gg = Nedbank_gg,
    Capitec_gg = Capitec_gg
  )
)

write_rds(artifacts_BA930, file = here("Outputs", "BA930", "artifacts_BA930.rds"))






