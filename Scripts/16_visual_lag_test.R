# Description
# Visual test for the model windows (lending volumes) - 22 February Xolani Sibande
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
library(corrplot)
library(janitor)

# econometrics
library(tseries)
library(strucchange)
library(vars)
library(urca)
library(mFilter)
library(car)

# Functions ---------------------------------------------------------------
source(here("Functions", "fx_plot.R"))
test_plot <- function(data,
                      index_1 = `draft index`,
                      index_2 = `implementation index`,
                      lag_string = "three",
                      plot_title = "add title",
                      axis_title = "add axis title"
) {
  data %>% 
    dplyr::select(date,
                  starts_with(lag_string)) %>% 
    pivot_longer(cols = -date, names_to = "series", values_to = "value") %>% 
    mutate(series = str_to_sentence(series)) %>%
    mutate(series = str_replace_all(series, "_", " ")) %>%
    ggplot(aes(x = date, y = value, color = series, alpha = 0.4)) +
    geom_line() +
    geom_line(data = data, 
              aes(x = date, y = {{index_1}}), 
              color = "black", alpha = 1) +
    geom_line(data = data, 
              aes(x = date, y = {{index_2}}), 
              color = "red", alpha = 1 ) +
    theme_bw() +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    theme(
      text = element_text(size = 8),
      strip.background = element_rect(colour = "white", fill = "white"),
      axis.text.x = element_text(angle = 90),
      axis.title = element_text(size = 7),
      plot.tag = element_text(size = 8)
    ) +
    labs(
         x = " ",
         y = axis_title) +
    facet_wrap(~series, scales = "free_y", ncol = 2) +
    scale_color_manual(values = pnw_palette("Sunset2", 9))
}

test_plot_2 <- function(data,
                     index_1 = `draft index`,
                     lag_string = "three",
                     plot_title = "add title",
                     axis_title = "add axis title"
) {
  data %>% 
    dplyr::select(date,
                  starts_with(lag_string)) %>% 
    pivot_longer(cols = -date, names_to = "series", values_to = "value") %>% 
    mutate(series = str_to_sentence(series)) %>%
    mutate(series = str_replace_all(series, "_", " ")) %>%
    ggplot(aes(x = date, y = value, color = series, alpha = 0.4)) +
    geom_line() +
    geom_line(data = data, 
              aes(x = date, y = {{index_1}}), 
              color = "black", alpha = 1) +
    theme_bw() +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    theme(
      text = element_text(size = 8),
      strip.background = element_rect(colour = "white", fill = "white"),
      axis.text.x = element_text(angle = 90),
      axis.title = element_text(size = 7),
      plot.tag = element_text(size = 8)
    ) +
    labs(
      x = " ",
      y = axis_title) +
    facet_wrap(~series, scales = "free_y", ncol = 2) +
    scale_color_manual(values = pnw_palette("Sunset2", 9))
}


# Import -------------------------------------------------------------
combined <- read_rds(here("Outputs", "combined_data", "artifacts_modelling_data.rds"))
combined_tbl <- 
  combined$combined_features_tbl %>% 
  clean_names() 

# Macropru test -----------------------------------------------------------
macropru_test_tbl <- 
  combined_tbl %>% 
  filter(banks == "Total Banks") %>% 
  dplyr::select(date, 
                draft, 
                implementation,
                starts_with("change_in"),
                starts_with("three")
                ) %>% 
  rename(`draft index` = draft,
         `implementation index` = implementation,
        ) 


one_month_test_macropru_gg <- test_plot(macropru_test_tbl,
                   lag_string = "change_in",
                   axis_title = "One month growth (%) / Index")

three_month_test_macropru_gg <- test_plot(macropru_test_tbl,
                   lag_string = "three",
                   axis_title = "Three month growth (%) / Index")

one_month_test_macropru_gg
three_month_test_macropru_gg


# Total macropru tests ----------------------------------------------------

three_month_test_macropru_totals_gg <- test_plot(macropru_test_tbl %>% dplyr::select(date, 
                                                                                     `draft index`,
                                                                                     `implementation index`,
                                                                                     contains("total_")),
                                          lag_string = "three",
                                          axis_title = "Three month growth (%) / Index")

# Competition test -----------------------------------------------------------
competition_test_tbl <- 
  combined_tbl %>% 
  filter(banks == "Total Banks") %>% 
  dplyr::select(date, 
                starts_with("change_in"),
                starts_with("three"),
                finance_regulation_dummy,
                financial_inclusion_dummy
  ) %>% 
  rename(`Finance regulation index` = finance_regulation_dummy,
         `Financial inclusion index` = financial_inclusion_dummy,
  )

one_month_test_finance_gg <- test_plot_2(competition_test_tbl,
                   index_1 = `Finance regulation index`,
                   lag_string = "change_in",
                   axis_title = "One month growth (%) / Index")

three_month_test_finance_gg <- test_plot_2(competition_test_tbl,
                   index_1 = `Finance regulation index`,
                   lag_string = "three",
                   axis_title = "Three month growth (%) / Index")

one_month_test_finance_gg
three_month_test_finance_gg


# Summarised competition test ---------------------------------------------
three_month_test_finance_total_gg <- test_plot(competition_test_tbl %>% dplyr::select(date,
                                                                                `Finance regulation index`,
                                                                                `Financial inclusion index`,
                                                                                contains("total_")),
                                         index_1 = `Finance regulation index`,
                                         index_2 = `Financial inclusion index`,
                                         lag_string = "three",
                                         axis_title = "Three month growth (%) / Index")


# Export ---------------------------------------------------------------
artifacts_visual_lag_test <- list(
  macropru = list(
  one_month_test_macropru_gg = one_month_test_macropru_gg,
  three_month_test_macropru_gg = three_month_test_macropru_gg,
  three_month_test_macropru_totals_gg = three_month_test_macropru_totals_gg
  ),
  finance = list(
  one_month_test_finance_gg = one_month_test_finance_gg,
  three_month_test_finance_gg = three_month_test_finance_gg,
  three_month_test_finance_total_gg = three_month_test_finance_total_gg
  )
)

write_rds(artifacts_visual_lag_test, file = here("Outputs","combined_data", "artifacts_visual_lag_test.rds"))


