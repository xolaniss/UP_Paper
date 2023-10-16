ba900_aggregration <-
  function(data) {
    data %>%
      pivot_wider(names_from = Series, values_from = Value) %>%
      mutate(
        `Non-financial corporate unsecured credit` =
          `Non-financial corporate sector credit cards` + # 168 
          `Non-financial corporate sector overdrafts` + # 182
          `Factoring debtors` + # 187
          `Non-financial corporate sector other loans and advances` # 190
      ) %>%
      mutate(
        `Non-financial corporate secured credit` =
          `Non-financial corporate sector instalment sales` + # 142
          `Non-financial corporate sector leasing transactions` # 147
      ) %>% 
    mutate(
      `Household sector unsecured credit` =
        `Households credit cards` + # 169
        `Households overdrafts` +  # 184
        `Households other loans and advances` + # 185
        `Non-profit organisations serving households overdrafts` + # 192
        `Non-profit organisations serving households other loans and advances` + # 193
        `Unincorporated business enterprises of households overdrafts` + #including 183
        `Unincorporated business enterprises of households other loans and advances` # including 191
    ) %>%
      mutate(`Household sector secured credit` =
               `Households leasing transactions` + # 148  # 143
               `Household sector instalment sales`) %>%
      mutate(
          `Non-financial corporate sector mortgages` =
          `Non-financial corporate sector farm mortgages` + # 152
          `Households farm mortgages` + # 153
          `Other farm mortgages` + # 154
          `Non-financial corporate sector residential mortgages` + # 156
          `Other residential mortgages` + # 158
          `Private non-financial corporate sector commercial and other mortgages` + # 163
          `Households commercial and other mortgages` + # 164
          `Other commercial and other mortgages` # 165
      ) %>%
      
      dplyr::select(
        Date,
        `Non-financial corporate unsecured credit`,
        `Non-financial corporate secured credit`,
        `Non-financial corporate sector mortgages`,
        `Household sector unsecured credit`,
        `Household sector secured credit`,
        `Households residential mortgages`,
        `Total assets`
      ) %>%
      mutate(`Our total` = rowSums(across(.cols = 2:6))) %>%
      mutate(`Other assets` = `Total assets` - `Our total`) %>%
      dplyr::select(-`Total assets`,-`Our total`)  %>%
      pivot_longer(-Date, names_to = "Series", values_to = "Value")
  }
