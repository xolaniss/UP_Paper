ba900_aggregration_shares <-
  function(filtered_data, aggregated_data) {
    filtered_data %>%
      pivot_wider(names_from = Series, values_from = Value) %>%
      left_join(aggregated_data %>% 
                  pivot_wider(names_from = Series, values_from = Value), 
                by = "Date") %>% 
      mutate(
         across(
           .cols  = c(
           `Non-financial corporate sector credit cards`,
             `Non-financial corporate sector overdrafts`, 
             `Non-financial corporate sector other loans and advances`), 
           .fns = ~ .x / `Non-financial corporate unsecured lending`,
           .names = "{col} share"
           )
         ) %>% 
      mutate(
        across(
          .cols  = c(
            `Households credit cards`,  
              `Households overdrafts` ,
              `Households other loans and advances`), 
          .fns = ~ .x / `Household unsecured lending`,
          .names = "{col} share"
        )
      ) %>% 
      mutate(
        across(
          .cols  = c(
            `Non-financial corporate unsecured lending`, 
            `Household unsecured lending`), 
          .fns = ~ .x / `Total unsecured lending`,
          .names = "{col} share"
        )
      ) %>%
      mutate(
        across(
          .cols  = c(
            `Non-financial corporate sector farm mortgages`, 
            `Non-financial corporate sector residential mortgages`,
            `Households farm mortgages`,
            `Private non-financial corporate sector commercial and other mortgages`,
            `Households commercial and other mortgages`),
          .fns = ~ .x /  `Commercial mortgages to corporates and households` ,
          .names = "{col} share"
        )
      ) %>%
      mutate(
        across(
          .cols  = c(
            `Commercial mortgages to corporates and households`, 
            `Residential mortgages to households`),
          .fns = ~ .x / `Total mortgage lending`,
          .names = "{col} share"
        )
      ) %>%
      mutate(
        across(
          .cols  = c(
            `Non-financial corporate sector instalment sales`, 
            `Non-financial corporate sector leasing transactions`),
          .fns = ~ .x / `Leasing and installments to corporates`,
          .names = "{col} share"
        )
      ) %>%
      mutate(
        across(
          .cols  = c(
            `Households leasing transactions`, 
            `Household sector instalment sales`),
          .fns = ~ .x / `Leasing and installments to households`,
          .names = "{col} share"
        )
      ) %>% 
      mutate(
        across(
          .cols  = c(
            `Leasing and installments to corporates`, 
            `Leasing and installments to households`),
          .fns = ~ .x / `Total leasing and installments` ,
          .names = "{col} share"
        )
      ) %>% 
      dplyr::select(
        Date,
        `Non-financial corporate sector credit cards share` ,                         
        `Non-financial corporate sector overdrafts share`,                            
        `Non-financial corporate sector other loans and advances share`,              
        `Households credit cards share` ,                                             
        `Households overdrafts share`,                                                
        `Households other loans and advances share`,                                  
        `Non-financial corporate unsecured lending share`,                            
        `Household unsecured lending share`,                                          
        `Non-financial corporate sector farm mortgages share` ,                       
        `Non-financial corporate sector residential mortgages share`,                 
        `Households farm mortgages share` ,                                           
        `Private non-financial corporate sector commercial and other mortgages share`,
        `Households commercial and other mortgages share`,                            
        `Commercial mortgages to corporates and households share` ,                   
        `Residential mortgages to households share`,                                  
        `Non-financial corporate sector instalment sales share`,                      
        `Non-financial corporate sector leasing transactions share` ,                 
        `Households leasing transactions share` ,                                     
        `Household sector instalment sales share` ,                                   
        `Leasing and installments to corporates share` ,                              
        `Leasing and installments to households share`   
      ) %>%
      # mutate(`Our total` = rowSums(across(.cols = 2:6))) %>%
      # mutate(`Other assets` = `Total assets` - `Our total`) %>%
      # dplyr::select(-`Total assets`,-`Our total`)  %>% Not necessary to isolate rest of assets
      pivot_longer(-Date, names_to = "Series", values_to = "Value")
  }
