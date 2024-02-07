model_workflow <-
function(data,
                           response_vec, 
                           predictor_vec){
  response_clean_vec <-  
    response_vec %>% 
    str_to_lower() %>% 
    str_replace_all(" ", "_")
    
  predictor_vec <- predictor_vec
  
  formula_list <- 
    glue("{response_clean_vec} ~ {glue_collapse(predictor_vec, sep = ' + ')}") %>% 
    as.list() %>% 
    map(~as.formula(.x))
 
   ols_model <- 
    map(formula_list, 
        ~ lm(.x, data = data)) 
   
    # set_names(response_clean_vec) 
 
   not_robust_models <- 
    ols_model
   
  robust_model <- 
    map(ols_model,
        .f = ~ coeftest(
          .x, 
          vcov = vcovBS,
          cluster = ~ banks
          ))
  robust_model

}
