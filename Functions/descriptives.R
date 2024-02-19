descriptives <-
function (data, group_var){
  data %>% 
    drop_na() %>%
    group_by(Series) %>%
    summarise(across(
      .cols = -c(Date),
      .fns = list(
        Median = median,
        SD = sd,
        Min = min,
        Max = max,
        IQR = IQR,
        Obs = ~ n()
      ),
      .names = "{.fn}"
    )) %>%
    mutate(Group = group_var)
}
