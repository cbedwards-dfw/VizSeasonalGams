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
  ## deal with offset
  term_names = names(terms)

  response = as.character(model$formula)[2]
  predictors_offset = grep("offset[(].*", names(terms), value = TRUE)
  pred_terms = terms[names(terms) != response]
  pred_terms = pred_terms[!grepl("offset[(].*", names(pred_terms))]

  predictors_factor = names(pred_terms)[pred_terms %in% c("factor", "character")]
  predictors_numeric = names(pred_terms)[pred_terms == "numeric"]

  return(list(response = response,
              predictors_offset = predictors_offset,
              predictors_factor = predictors_factor,
              predictors_numeric = predictors_numeric))
}
