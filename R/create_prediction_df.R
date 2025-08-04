#' Create prediction dataframe
#'
#' From an MGCV model, create a dataframe (tibble) of values to predict across, including
#' all combinations of categorical predictors in the model, and incrementing across
#' the `plot_by` variable, and using median values of other numeric predictors.
#' Used by `predict_gam()` -- see there for more details
#'
#' @inheritParams predict_gam
#'
#' @return tibble of variables relevant for the provided model, with all relevant combinations of
#' variable values.
#' @export
#'
create_prediction_df = function(model,
                                plot_by = "doy",
                                across_increment = 1,
                                quant_trimming = 0.01,
                                verbose = TRUE){

  validate_model(model)
  validate_variable(plot_by, names(model$model))
  validate_number(across_increment)
  validate_number(quant_trimming, limits_inclusive = c(0,0.499))
  validate_flag(verbose)

  ## parse terms
  terms_ls = VizSeasonalGams::parse_terms(model)
  predictors_factor = terms_ls$predictors_factor
  predictors_numeric = setdiff(terms_ls$predictors_numeric, plot_by)
  response = terms_ls$response

  dat_model = model$model |>
    dplyr::select(-tidyselect::any_of(response))

  helper_sequencer = function(dat,
                              plot_by_var = plot_by){tidyr::expand_grid(seq(stats::quantile(dat[[plot_by_var]], quant_trimming),
                                                                            stats::quantile(dat[[plot_by_var]], 1 - quant_trimming),
                                                                            by = across_increment))}

  pred_df = dat_model |>
    dplyr::select(tidyselect::any_of(c(predictors_factor, plot_by))) |>
    tidyr::nest(.by = tidyselect::any_of(predictors_factor)) |>
    dplyr::mutate(pred_mats = purrr::map(.data$data, helper_sequencer)) |>
    dplyr::select(-tidyselect::any_of("data")) |>
    tidyr::unnest("pred_mats")
  names(pred_df)[ncol(pred_df)] = plot_by

  predictors_list = list()
  if(verbose & length(predictors_numeric)>0){
    cli::cli_alert("Using median values of {predictors_numeric} across ALL factor predictors_")
  }
  if(length(predictors_numeric) > 0 ){
    for(i in 1:length(predictors_numeric)){
      predictors_list[[predictors_numeric[i]]] = stats::median(dat_model[[predictors_numeric[i]]])
    }
  }
  pred_df_numerics = do.call(tidyr::expand_grid, predictors_list)

  pred_df = tidyr::expand_grid(pred_df, pred_df_numerics)
  return(pred_df)
}
