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
                       plot_by = "doy",
                       across_increment = 1,
                       quant_trimming = 0.01,
                       verbose = TRUE){ ## Defines quantile-based trimming before predicting the `plot_by` variable.
  ##                    For example, if this is 0.01 and plot_by is "doy", predictions will range between the 0.01 and 0.99 quantiles of observed doy within each combination of factor predictors
  validate_model(model)
  validate_variable(plot_by, names(model$model))
  validate_number(across_increment)
  validate_number(quant_trimming, limits_inclusive = c(0,0.499))
  validate_flag(verbose)



  pred_df = create_prediction_df(model,
                       plot_by = plot_by,
                       across_increment = across_increment,
                       quant_trimming = quant_trimming,
                       verbose = verbose)

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
