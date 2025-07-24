#' Wrap panels from seasonal GAM plots
#'
#' @description
#' Analog to facet_wrap() for the seasonal gam panels. If there are two predictors
#' of interest, consider `grid_panels()` instead.
#'
#' @param panel_df A dataframe from plot_seasonal_gam_panels.
#' @param ncol Optional. Force a number of columns. Numeric, defaults to NULL.
#' @param nrow Optional. Force a number of rows. Numeric, defaults to NULL.
#' @param title_size size of panel title text. Numeric, defaults to 8.
#'
#' @returns
#' A wrapped plot object with row and column labels.
#'
#' @export
wrap_panels = function(panel_df, ## dataframe from plot_seasonal_gam_panels
                       ncol = NULL, ## number of columns to enforce
                       nrow = NULL,
                       title_size = 8){ ## number of rows to enforce

  terms_constant = names(panel_df)[apply(panel_df, 2, function(x){length(unique(x))})==1]

  title_terms = panel_df |>
    dplyr::select(-tidyselect::any_of(".plot")) |>
    dplyr::select(-tidyselect::any_of(terms_constant))

  annotation_text = panel_df |>
    dplyr::select(tidyselect::all_of(terms_constant)) |>
    dplyr::distinct()|>
    dplyr::mutate(dplyr::across(tidyselect::everything(), ~as.character(.x))) |>
    unlist()
  annotation_text = paste0(paste0(names(annotation_text), ": ", annotation_text), collapse = " | ")

  for(i in 1:nrow(panel_df)){
    title_text = title_terms[i, ] |>
      dplyr::mutate(dplyr::across(tidyselect::everything(), ~as.character(.x))) |>
      unlist()

    title_text = paste0(paste0(names(title_text), ": ", title_text), collapse = " | ")

    if("patchwork" %in% class(panel_df$.plot[[i]])){
      panel_df$.plot[[i]][[1]] =
        plot.modified = panel_df$.plot[[i]][[1]] +
        ggplot2::ggtitle(title_text)+
        ggplot2::theme(plot.title = ggplot2::element_text(size = title_size))
    } else {
      panel_df$.plot[[i]] =
        plot.modified = panel_df$.plot[[i]] +
        ggplot2::ggtitle(title_text)+
        ggplot2::theme(plot.title = ggplot2::element_text(size = title_size))
    }
  }

  res = patchwork::wrap_plots(panel_df$.plot, ncol = ncol, nrow = nrow)+
    plot_layout(guides = "collect")

  if(length(terms_constant) > 0){
    res = res + patchwork::plot_annotation(subtitle = annotation_text)
  }

  return(res)
}


