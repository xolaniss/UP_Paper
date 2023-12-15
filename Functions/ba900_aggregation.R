ba900_aggregration <-
  function(data) {
    data %>%
      pivot_wider(names_from = Series, values_from = Value) %>%
      mutate(
        `Non-financial corporate unsecured lending` =
          `Non-financial corporate sector credit cards` + # 168
          `Non-financial corporate sector overdrafts` + # 183
          `Non-financial corporate sector other loans and advances`  # 190
      ) %>%
      mutate(
         `Household unsecured lending` =
          `Households credit cards` + # 169
          `Households overdrafts` + # 185
          `Households other loans and advances`  # 192
      ) %>%
      mutate(
        `Total unsecured lending` =
          `Non-financial corporate unsecured lending` +
          `Household unsecured lending`
      ) %>% 
      mutate(
        `Commercial mortgages to corporates and households` =
         `Non-financial corporate sector farm mortgages` + # 152
          `Households farm mortgages` + # 153
          `Non-financial corporate sector residential mortgages` + # 156
          `Private non-financial corporate sector commercial and other mortgages` + # 163
          `Households commercial and other mortgages` # 164
      ) %>%
      mutate(
        `Residential mortgages to households` =
          `Households residential mortgages`  # 157
      ) %>% 
      mutate (
        `Total mortgage lending` = 
          `Commercial mortgages to corporates and households` +
          `Residential mortgages to households`
      ) %>%
      mutate(
        `Leasing and installments to corporates` =
          `Non-financial corporate sector instalment sales` + # 142
          `Non-financial corporate sector leasing transactions`  # 147
      ) %>% 
      mutate(
        `Leasing and installments to households` =
          `Household sector instalment sales` + # 143
          `Households leasing transactions`  # 148
      ) %>% 
      mutate(
        `Total leasing and installments` =
          `Leasing and installments to corporates` +
          `Leasing and installments to households`
      ) %>%
      dplyr::select(
        Date,
        `Total unsecured lending`,
        `Non-financial corporate unsecured lending`,
        `Household unsecured lending`,
        `Total mortgage lending`,
        `Commercial mortgages to corporates and households`,
        `Residential mortgages to households`,
        `Total leasing and installments`,
        `Leasing and installments to corporates`,
        `Leasing and installments to households`
      ) %>%
      # mutate(`Our total` = rowSums(across(.cols = 2:6))) %>%
      # mutate(`Other assets` = `Total assets` - `Our total`) %>%
      # dplyr::select(-`Total assets`,-`Our total`)  %>% Not necessary to isolate rest of assets
      pivot_longer(-Date, names_to = "Series", values_to = "Value")
  }
