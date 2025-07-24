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

  terms = attr(terms(model), "dataClasses")
  response = as.character(model$formula)[2]
  pred_terms = terms[names(terms) != response]

  predictors_factor = names(pred_terms)[pred_terms == "factor"]
  predictors_numeric = names(pred_terms)[pred_terms == "numeric"]

  return(list(response = response,
              predictors_factor = predictors_factor,
              predictors_numeric = predictors_numeric))
}
