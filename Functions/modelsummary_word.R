modelsummary_word <- function(models = models,
                              title = "Insert title", 
                              coef_map = NULL, 
                              coef_rename  = NULL,
                              stars = TRUE, 
                              decimals = 2,
                              vcov =  NULL,
                              variables_omit = "AIC|BIC|Log|RMSE|Std|Adj"
) {
  modelsummary(
    models,
    output = "flextable",
    stars = stars,
    fmt = decimals,
    vcov =  vcov,
    title = title,
    gof_omit = variables_omit ,
    coef_map = coef_map,
    coef_rename = coef_rename
    
  ) %>%
    theme_booktabs() %>%
    border_inner(border = fp_border(color = "white"), part = "all") 
}
