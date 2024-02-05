formula_function <-
function(
    response_vec = NULL,
    predictor_vec = NULL
  ) {
    as.formula(paste(
      response_vec,
      paste(predictor_vec, collapse = "+"),
      sep = " ~ "
    ))
  }
