#' Predict GAM across reasonable values
#'
#' @description
#' Predicts gam output. Primarily an internal tool, used in `plot_seasonal_gam_panels.R`.
#'
#' @inheritParams plot_seasonal_gam_panels
#'
#' @returns
#' A data frame with predictions, confidence intervals, and predictor values.
#'
#' @export
#'
predict_gam = function(model,
                       plot_across = "doy",
                       across_increment = 1,
                       quant_trimming = 0.01,
                       verbose = TRUE){ ## Defines quantile-based trimming before predicting the `plot_across` variable.
  ##                    For example, if this is 0.01 and plot_across is "doy", predictions will range between the 0.01 and 0.99 quantiles of observed doy within each combination of factor predictors
  validate_model(model)
  validate_flag(verbose)

  if(!(is.numeric(across_increment) & length(across_increment) == 1)){
    cli::cli_abort("`across_increment` must be a numeric of length 1.")
  }

  if(!(is.numeric(quant_trimming) & length(quant_trimming) == 1)){
    cli::cli_abort("`quant_trimming` must be a numeric of length 1.")
  }

  ## parse terms
  terms_ls = VizSeasonalGams::parse_terms(model)
  predictors_factor = terms_ls$predictors_factor
  predictors_numeric = setdiff(terms_ls$predictors_numeric, plot_across)
  response = terms_ls$response

  dat_model = model$model |>
    dplyr::select(-tidyselect::any_of(response))

  helper_sequencer = function(dat,
                              plot_across_var = plot_across){tidyr::expand_grid(seq(stats::quantile(dat[[plot_across_var]], quant_trimming),
                                                          stats::quantile(dat[[plot_across_var]], 1 - quant_trimming),
                                                          by = across_increment))}

  pred_df = dat_model |>
    dplyr::select(tidyselect::any_of(c(predictors_factor, plot_across))) |>
    tidyr::nest(.by = tidyselect::any_of(predictors_factor)) |>
    dplyr::mutate(pred_mats = purrr::map(.data$data, helper_sequencer)) |>
    dplyr::select(-tidyselect::any_of("data")) |>
    tidyr::unnest("pred_mats")
  names(pred_df)[ncol(pred_df)] = plot_across

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

  ##just in case of factor response
  ## handle discrete predictors
  ## handle non-across continuous predictors
  ## then handle across predictors
  pred_raw = stats::predict(model, newdata = pred_df, type = "link", se.fit = TRUE)
  pred_df$prediction = model$family$linkinv( pred_raw$fit )
  pred_df$ci_low = as.numeric(model$family$linkinv( pred_raw$fit - 1.96 * pred_raw$se.fit ))
  pred_df$ci_high = as.numeric(model$family$linkinv( pred_raw$fit + 1.96 * pred_raw$se.fit ))
  return(pred_df)
}
