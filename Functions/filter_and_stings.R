filter_and_strings <-
function(data, ...){
  data %>% 
      filter(Series %in% ..., .preserve = TRUE)  %>% 
      mutate(Series = str_to_sentence(Series))
}
