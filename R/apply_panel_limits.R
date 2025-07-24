

apply_panel_limits = function(panels_df,
                              scales_x,
                              scales_y,
                              column_var,
                              row_var,
                              scales_y_coverage = "free"){

  validate_scale(scales_x)
  validate_scale(scales_y)
  validate_variable(column_var, setdiff(names(panels_df), ".plot"))
  validate_variable(row_var, setdiff(names(panels_df), ".plot"))
  scales_y_coverage = validate_scale(scales_y_coverage)



  if("patchwork" %in% class(panels_df$.plot[[1]])){
    breaks_x = ggplot2::ggplot_build(panels_df$.plot[[1]])$layout$panel_scales_x[[1]]$n.breaks
    labeler_x = ggplot2::ggplot_build(panels_df$.plot[[1]])$layout$panel_scales_x[[1]]$labels

    panels_df <- panels_df |>
      dplyr::mutate(xlim = purrr::map(.data$.plot, extract_xlim_patchwork)) |>
      tidyr::unnest(.data$xlim) |>
      dplyr::mutate(ylim = purrr::map(.data$.plot, extract_ylim_patchwork)) |>
      tidyr::unnest(.data$ylim) |>
      dplyr::mutate(ylim_coverage = purrr::map(.data$.plot, extract_ylim_coverage_patchwork)) |>
      tidyr::unnest(.data$ylim_coverage)
  } else {
    breaks_x = ggplot2::ggplot_build(panels_df$.plot)$layout$panel_scales_x[[1]]$n.breaks
    labeler_x = ggplot2::ggplot_build(panels_df$.plot)$layout$panel_scales_x[[1]]$labels
    panels_df <-  panels_df |>
      dplyr::mutate(xlim = purrr::map(.data$.plot, extract_xlim_ggplot)) |>
      tidyr::unnest(.data$xlim) |>
      dplyr::mutate(ylim = purrr::map(.data$.plot, extract_ylim_ggplot)) |>
      tidyr::unnest(.data$ylim)
  }

  ## apply scales_x, default is "free", otherwise modify
  if(scales_x == "fixed_column"){
    panels_df = panels_df |>
      dplyr::group_by(.data[[column_var]]) |>
      dplyr::mutate(xlim_low = min(.data$xlim_low),
                    xlim_high = max(.data$xlim_high)) |>
      dplyr::ungroup()
  }
  if(scales_x == "fixed_row"){
    panels_df = panels_df |>
      dplyr::group_by(.data[[row_var]]) |>
      dplyr::mutate(xlim_low = min(.data$xlim_low),
                    xlim_high = max(.data$xlim_high)) |>
      dplyr::ungroup()
  }
  if(scales_x == "fixed_combination"){
    panels_df = panels_df |>
      dplyr::group_by(.data[[row_var]],
                      .data[[column_var]]) |>
      dplyr::mutate(xlim_low = min(.data$xlim_low),
                    xlim_high = max(.data$xlim_high)) |>
      dplyr::ungroup()
  }
  if(scales_x == "fixed_all"){
    panels_df = panels_df |>
      dplyr::mutate(xlim_low = min(.data$xlim_low),
                    xlim_high = max(.data$xlim_high))
  }

  ## apply scales_y, default is "free"
  if(scales_y == "fixed_column"){
    panels_df = panels_df |>
      dplyr::group_by(.data[[column_var]]) |>
      dplyr::mutate(ylim_low = min(.data$ylim_low),
                    ylim_high = max(.data$ylim_high)) |>
      dplyr::ungroup()
  }
  if(scales_y == "fixed_row"){
    panels_df = panels_df |>
      dplyr::group_by(.data[[row_var]]) |>
      dplyr::mutate(ylim_low = min(.data$ylim_low),
                    ylim_high = max(.data$ylim_high)) |>
      dplyr::ungroup()
  }
  if(scales_y == "fixed_combination"){
    panels_df = panels_df |>
      dplyr::group_by(.data[[row_var]],
                      .data[[column_var]]) |>
      dplyr::mutate(ylim_low = min(.data$ylim_low),
                    ylim_high = max(.data$ylim_high)) |>
      dplyr::ungroup()
  }
  if(scales_y == "fixed_all"){
    panels_df = panels_df |>
      dplyr::mutate(ylim_low = min(.data$ylim_low),
                    ylim_high = max(.data$ylim_high))
  }

  ## apply scales_y_coverage, default is "free"
  if("patchwork" %in% class(panels_df$.plot[[1]])){
    if(scales_y_coverage == "fixed_column"){
      panels_df = panels_df |>
        dplyr::group_by(.data[[column_var]]) |>
        dplyr::mutate(ylim_coverage_low = min(.data$ylim_coverage_low),
                      ylim_coverage_high = max(.data$ylim_coverage_high)) |>
        dplyr::ungroup()
    }
    if(scales_y_coverage == "fixed_row"){
      panels_df = panels_df |>
        dplyr::group_by(.data[[row_var]]) |>
        dplyr::mutate(ylim_coverage_low = min(.data$ylim_coverage_low),
                      ylim_coverage_high = max(.data$ylim_coverage_high)) |>
        dplyr::ungroup()
    }
    if(scales_y_coverage == "fixed_combination"){
      panels_df = panels_df |>
        dplyr::group_by(.data[[row_var]],
                        .data[[column_var]]) |>
        dplyr::mutate(ylim_coverage_low = min(.data$ylim_coverage_low),
                      ylim_coverage_high = max(.data$ylim_coverage_high)) |>
        dplyr::ungroup()
    }
    if(scales_y_coverage == "fixed_all"){
      panels_df = panels_df |>
        dplyr::mutate(ylim_coverage_low = min(.data$ylim_coverage_low),
                      ylim_coverage_high = max(.data$ylim_coverage_high))
    }
  }

  if("patchwork" %in% class(panels_df$.plot[[1]])){
    panels_df = panels_df |>
      dplyr::mutate(.plot = purrr::pmap(list(plot = .data$.plot,
                                             xlim_low = .data$xlim_low,
                                             xlim_high = .data$xlim_high,
                                             ylim_low = .data$ylim_low,
                                             ylim_high = .data$ylim_high,
                                             ylim_coverage_low = .data$ylim_coverage_low,
                                             ylim_coverage_high = .data$ylim_coverage_high,
                                             breaks_x = rep(list(breaks_x), nrow(panels_df)),
                                             labeler_x = rep(list(labeler_x), nrow(panels_df))),
                                        revise_limits_patchwork)) |>
      dplyr::select(-tidyselect::any_of(c("xlim_low", "xlim_high", "ylim_low", "ylim_high", "ylim_coverage_low", "ylim_coverage_high")))
  } else {
    panels_df = panels_df |>
      dplyr::mutate(.plot = purrr::pmap(list(plot = .data$.plot,
                                             xlim_low = .data$xlim_low,
                                             xlim_high = .data$xlim_high,
                                             ylim_low = .data$ylim_low,
                                             ylim_high = .data$ylim_high,
                                             breaks_x = rep(list(breaks_x), nrow(panels_df)),
                                             labeler_x = rep(list(labeler_x), nrow(panels_df))),
                                        revise_limits_ggplot)) |>
      dplyr::select(-tidyselect::any_of(c("xlim_low", "xlim_high", "ylim_low", "ylim_high", "ylim_coverage_low", "ylim_coverage_high")))
  }
  return(panels_df)

}


revise_limits_patchwork = function(plot, xlim_low, xlim_high, ylim_low, ylim_high, ylim_coverage_low, ylim_coverage_high, breaks_x, labeler_x){
  plot[[1]] = suppressMessages(plot[[1]] + ggplot2::scale_x_continuous(limits = c(xlim_low, xlim_high),
                                                                       n.breaks = breaks_x,
                                                                       labels = labeler_x) +
                                 ggplot2::ylim(ylim_low, ylim_high))
  plot[[2]] = suppressMessages(plot[[2]] + ggplot2::scale_x_continuous(limits = c(xlim_low, xlim_high),
                                                                       n.breaks = breaks_x,
                                                                       labels = labeler_x) +
                                 ggplot2::scale_y_continuous(limits = c(ylim_coverage_low, ylim_coverage_high),
                                                             breaks = c(ylim_coverage_low, ylim_coverage_high)) )
  return(plot)
}

revise_limits_ggplot = function(plot, xlim_low, xlim_high, ylim_low, ylim_high){
  suppressMessages(plot[[1]] + ggplot2::xlim(c(xlim_low, xlim_high)) + ggplot2::ylim(ylim_low, ylim_high))
}
