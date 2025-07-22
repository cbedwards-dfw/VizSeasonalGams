#' Parse model terms
#'
#' @description
#' Provides names of the model response and predictors
#'
#' @param model An mgcv model object.
#'
#' @returns
#' A list with components `response` (the response variable name),
#' `predictors_factor` (names of factor/character predictors), and
#' `predictors_numeric` (names of numeric predictors).
#'
#' @export

parse_terms = function(model){
  validate_model(model)

  all_terms = names(model$var.summary)
  response = as.character(model$formula)[2]
  dat_model = model$model |>
    dplyr::select(-dplyr::any_of(response))

  predictors = setdiff(all_terms, response)
  predictors_factor = names(dat_model)[sapply(dat_model, function(x){is.factor(x) | is.character(x)})]
  predictors_numeric = setdiff(predictors, predictors_factor)
  return(list(response = response,
              predictors_factor = predictors_factor,
              predictors_numeric = predictors_numeric))
}
