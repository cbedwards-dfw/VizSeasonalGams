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
  ## deal with offsets
  term_names = names(terms)
  if(length(grep("offset", term_names))>0){
    ind_offsets = grep("offset", term_names)
    term_names[ind_offsets] = gsub(".*[(]", "", term_names[ind_offsets])
    term_names[ind_offsets] = gsub("[)].*", "", term_names[ind_offsets])
    names(terms) = term_names
  }

  response = as.character(model$formula)[2]
  pred_terms = terms[names(terms) != response]

  predictors_factor = names(pred_terms)[pred_terms %in% c("factor", "character")]
  predictors_numeric = names(pred_terms)[pred_terms == "numeric"]

  return(list(response = response,
              predictors_factor = predictors_factor,
              predictors_numeric = predictors_numeric))
}
