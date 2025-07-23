#' Create tibble of predictors and prediction plots
#'
#' @description
#'
#' Creates a tibble with all represented combinations of factor variables in the data,
#' and includes a model of model predictions across a key continuous variable (`plot_by`, defaults to "doy")
#' while setting other continuous variables to their median. Because these plots are within a tibble framework, it is easy to filter to desired combinations of
#' variables and plot those. (e.g., model might fit 20 years for 13 spatial units, tibble
#' can rapidly be filtered to just the years of a desired spatial unit, or all spatial units
#' of a desired year).
#'
#' @param model A GAM model object from mgcv
#' @param color_by Variable to color predictions by. Defaults to NULL, in which case curves are black and each panel has only one set of predictions. When used, color_by necessarily reduces dimensionalality of output by 1.
#' @param plot_by A single string specifying the variable to plot across. Defaults to "doy".
#' @param across_increment A numeric value specifying the increment for the plot_by variable. Defaults to 1.
#' @param quant_trimming A numeric value between 0 and 1 defining quantile-based trimming -- this provides a buffer between the outmost observations and the outmost plot predictions. Defaults to 0.01; increase if plot predictions at the edges are misleading.
#' @param include_cis Should we plot the confidence envelope? Logical, defaults to TRUE
#' @param breaks_x Approximate number of ticks to use on the X axis, useful to ensure text is legible. Numeric, defaults to 3. Because of quirks in how ggplot works (and the patchwork combination process), plots may not show exactly `breaks_x` number of ticks, but you can still manipulate actual tick number by changing `breaks_x`.
#' @param plot_coverage Add histogram at bottom of each panel with data coverage? Logical, defaults to TRUE.
#' @param verbose Provide context? Logical, defaults to TRUE
#'
#' @return Tibble, with a column for each factor predictor in `model`. `$plot` column contains the plot panels.
#' @export
#'
plot_seasonal_gam_panels = function(model, ## fitted gam model
                                    color_by = NULL, #name of variable to color terms by
                                    plot_by = "doy", #name of variable to use as the x axis
                                    across_increment = 1,
                                    quant_trimming = 0.01,
                                    include_cis = TRUE,
                                    # plot_observations = FALSE, # If TRUE, add data points to plot
                                    plot_coverage = TRUE,
                                    breaks_x = 3,
                                    verbose = TRUE){ ## If TRUE, add small histogram of data coverage along bottom of each panel

  validate_model(model)
  if(!is.null(color_by)){
    validate_variable(color_by, names(model$model))
  }
  validate_variable(plot_by, names(model$model))
  validate_number(across_increment, limits_exclusive = c(0, NA))
  validate_number(quant_trimming, limits_exclusive = c(0, 0.5))
  validate_flag(include_cis)
  validate_flag(plot_coverage)
  validate_integer(breaks_x, limits_exclusive = c(0, NA))
  validate_flag(verbose)


  terms_ls = VizSeasonalGams::parse_terms(model)
  predictors_factor = terms_ls$predictors_factor
  predictors_numeric = setdiff(terms_ls$predictors_numeric, plot_by)
  response = terms_ls$response

  if(is.null(color_by) & verbose){
    cli::cli_alert("`color_by` not provided. Defaulting to no color.")
    cli::cli_alert("Options for `color_by`: {predictors_factor}.")
  }

  dat_model = model$model

  dat_pred = predict_gam(model, plot_by = plot_by,
                         across_increment = across_increment,
                         quant_trimming = quant_trimming,
                         verbose = verbose)

  #
  if(!is.null(color_by)){
  predictors_panel = setdiff(predictors_factor, color_by)
  } else {
    predictors_panel = predictors_factor
  }

  panel_combinations = dat_pred |>
    dplyr::select(dplyr::all_of(predictors_panel)) |>
    dplyr::distinct()
  panel_combinations$plot = as.list(rep(NA, nrow(panel_combinations)))

  for(i in 1:nrow(panel_combinations)){
    dat_panel = dplyr::left_join(panel_combinations[i, ],
                          dat_pred,
                          by = predictors_panel)
    panel_details = panel_combinations[i, ] |>
      dplyr::select(-tidyselect::any_of("plot")) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.character(.x))) |>
      unlist()

    if(!(is.null(color_by))){
    gp = dat_panel |>
      ggplot2::ggplot(ggplot2::aes(x = .data[[plot_by]], y = .data$prediction, col = .data[[color_by]], fill = .data[[color_by]])) +
      ggplot2::geom_path(linewidth = 0.8)+
      ggplot2::scale_x_continuous(labels = VizSeasonalGams::doy_2md,
                                  n.breaks = breaks_x)+ ## FRAGILE -- can't handle non-date axis
      ggplot2::labs(y = response,
           x = "",
           title = paste(paste(names(panel_details), ": ", panel_details), collapse = "\n"))
    } else {
      gp = dat_panel |>
        ggplot2::ggplot(ggplot2::aes(x = .data[[plot_by]], y = .data$prediction)) +
        ggplot2::geom_path(linewidth = 0.8)+
        ggplot2::scale_x_continuous(labels = VizSeasonalGams::doy_2md,
                                    n.breaks = breaks_x)+ ## FRAGILE -- can't handle non-date axis
        ggplot2::labs(y = response,
                      x = "",
                      title = paste(paste(names(panel_details), ": ", panel_details), collapse = "\n"))
    }
    if(include_cis){
      gp = gp + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$ci_low, ymax = .data$ci_high), alpha = 0.1)
    }


    ## add histogram under-panel if desired
    if(plot_coverage){
      dat_rugs = dplyr::left_join(panel_combinations[i, ],
                           dat_model,
                           by = predictors_panel) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(c(predictors_panel, plot_by)))) |>
        dplyr::summarize(n = dplyr::n(), .groups = "drop") |>
        dplyr::ungroup()

      hist_gp = dat_rugs |>
        # filter(_data[[facet_by]] == cur_panel$data[[facet_by]][1]) |>
        ggplot2::ggplot(ggplot2::aes(x = .data[[plot_by]], y = .data$n))+
        ggplot2::scale_y_continuous(n.breaks = 2)+
        ggplot2::geom_col(na.rm = TRUE)+
        ggplot2::scale_x_continuous(limits = ggplot2::layer_scales(gp)$x$range$range,
                           labels = VizSeasonalGams::doy_2md,
                           n.breaks = breaks_x)+ ## FRAGILE -- can't handle non-date axis
        ggplot2::labs(x = "")
      gp = gp / hist_gp  +
        patchwork::plot_layout(heights = c(10,1), axis_title = "collect", axes = "collect_x")
    }

    panel_combinations$plot[[i]] = gp

  }

  return(panel_combinations)
}
