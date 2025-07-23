## facet_grid but for our sweet gam fits
##  will also handle
##    (a) missing panels (e.g., we had not data for a given combination of factor predictors)
##    (b) subpanels: multiple terms in the panels_df that match a single combination of column + row variables.
#' Title
#'
#' @param panels_df A dataframe from plot_seasonal_gam_panels.
#' @param column_var 1 dimensional character vector of variable defining the columns of the grid. Must be factor variable present in `panels_df`.
#' @param scales_x Allows shared x limits for all panels within each column ("fixed_column"), or each row ("fixed_row"), each column x row combinations ("fixed_combination"; useful when more than one panel matches a row x column criterion). Defaults to allowing each panel to have its own x scale ("free").
#' @param scales_y As `scales_x`, but for y limits. Is not applied to the optional coverage plots within each panel.
#' @param scales_y_coverage As `scales_y`, but only applied to optional coverage plots.
#' @param row_var as `column_var`, but defines the rows of the grid
#' @param panel_titles Include all panel information in panel title even if redundant with rows/panels? Useful for debugging or screenshotting sections. Logical, defaults to false
#' @param col_title_ratio How much bigger should content be than the column labels? Numeric, defaults to 30.
#' @param row_title_ratio as `col_title_ratio`, but ratio of content to row labels.
#' @param font_size Size of row and column labels. Numeric, defaults to 18.
#' @param new_y_label Optional, replaces Y axis label of primary plots in panels (not data coverage histograms). character vector, defaults to NULL.
#' @param new_x_label Optional, replaces X axis label of panels. Character vector, defaults to NULL.
#' @param title_size size of panel title text. Numeric, defaults to 8.
#'
#' @return patchwork object
#' @export
#'
grid_panels = function(
    panels_df,
    column_var,
    row_var,
    scales_x = c("free", "fixed_column", "fixed_row", "fixed_all", "fixed_combination"),
    scales_y = c("free", "fixed_column", "fixed_row", "fixed_all", "fixed_combination"),
    scales_y_coverage = c("free", "fixed_column", "fixed_row", "fixed_all", "fixed_combination"),
    panel_titles = FALSE,
    col_title_ratio = 30,
    row_title_ratio = 30,
    font_size = 18,
    new_y_label = NULL,
    new_x_label = NULL,
    title_size = 8

){
  ## validation
  validate_variable(column_var, setdiff(names(panels_df), "plot"))
  validate_variable(row_var, setdiff(names(panels_df), "plot"))

  scales_x = validate_scale(scales_x)
  scales_y = validate_scale(scales_y)
  scales_y_coverage = validate_scale(scales_y_coverage)

  validate_flag(panel_titles)
  validate_number(col_title_ratio, limits_exclusive = c(0, NA))
  validate_number(row_title_ratio, limits_exclusive = c(0, NA))
  validate_number(font_size, limits_exclusive = c(0, NA))
  validate_optional_char(new_y_label)
  validate_optional_char(new_x_label)
  validate_number(title_size, limits_exclusive = c(0, NA))


  panels_df = apply_panel_limits(panels_df,
                                 scales_x = scales_x,
                                 scales_y = scales_y,
                                 column_var = column_var,
                                 row_var = row_var,
                                 scales_y_coverage = scales_y_coverage)

  row_vals = sort(unique(panels_df[[row_var]]))
  column_vals = sort(unique(panels_df[[column_var]]))

  ## calculating plot dimensions to support scales_x and scales_y

  list_figs = rep(list(ggplot2::ggplot() + ggplot2::theme_void()),
                  length(row_vals) * length(column_vals))


  for(i_row in 1:length(row_vals)){
    for(i_column in 1:length(column_vals)){
      ind = (i_row - 1) * length(column_vals) + i_column

      cur_plot = panels_df |>
        dplyr::filter(.data[[column_var]] == as.character(column_vals[i_column]),
                      .data[[row_var]] == as.character(row_vals[i_row]))


      if(nrow(cur_plot) == 1 & !panel_titles){
        ## overwrite lone wolf titles
        ## index depth depends on if individual plots are patchworks with histogram panel or not
        if("patchwork" %in% class(cur_plot$plot[[1]])){
          cur_plot$plot[[1]][[1]] = cur_plot$plot[[1]][[1]] + ggplot2::ggtitle("")
        } else {
          cur_plot$plot[[1]] = cur_plot$plot[[1]] + ggplot2::ggtitle("")
        }
      }


      ## updating x and y labels
      if(any(!is.null(new_x_label) | !is.null(new_y_label))){
        for(i_subplot in 1:length(plot_labels)){
          if(!is.null(new_y_label)){
            if("patchwork" %in% class(cur_plot$plot[[1]])){
              cur_plot$plot[[i_subplot]][[1]] = cur_plot$plot[[i_subplot]][[1]] + ggplot2::ylab(new_y_label)
            } else {
              cur_plot$plot[[i_subplot]] = cur_plot$plot[[i_subplot]] + ggplot2::ylab(new_y_label)
            }
          }

          if(!is.null(new_x_label)){
            if("patchwork" %in% class(cur_plot$plot[[1]])){
              cur_plot$plot[[i_subplot]][[1]] = cur_plot$plot[[i_subplot]][[1]] + ggplot2::xlab(new_x_label)
              cur_plot$plot[[i_subplot]][[2]] = cur_plot$plot[[i_subplot]][[2]] + ggplot2::xlab(new_x_label)
            } else {
              cur_plot$plot[[i_subplot]] = cur_plot$plot[[i_subplot]] + ggplot2::xlab(new_x_label)
            }
          }
        }
      }

      if(nrow(cur_plot) > 1 & !panel_titles){
        # rework multi-subpanel titles
        ## new titles
        vars_subplot = setdiff(names(cur_plot), c(column_var, row_var))
        vars_subplot = setdiff(vars_subplot, "plot")
        plot_labels = cur_plot |>
          dplyr::select(tidyselect::any_of(vars_subplot)) |>
          dplyr::mutate(dplyr::across(tidyselect::everything(), ~as.character(.x)))
        for(col_cur in names(plot_labels)){
          plot_labels[[col_cur]] = paste0(col_cur, ": ", plot_labels[[col_cur]])
        }
        plot_labels = apply(plot_labels, 1, paste0, collapse = "\n")

        ## replace the titles
        for(i_subplot in 1:length(plot_labels)){
          if("patchwork" %in% class(cur_plot$plot[[1]])){
            cur_plot$plot[[i_subplot]][[1]] = cur_plot$plot[[i_subplot]][[1]] + ggplot2::ggtitle(plot_labels[i_subplot])
          } else {
            cur_plot$plot[[i_subplot]] = cur_plot$plot[[i_subplot]] + ggplot2::ggtitle(plot_labels[i_subplot])
          }
        }
      }

      if(nrow(cur_plot) >0){
        list_figs[[ind]] = cur_plot |>
          dplyr::pull(.data$plot) |>
          patchwork::wrap_plots()
      }
    }
  }

  panels_matrix = patchwork::wrap_plots(list_figs,
                                        ncol = length(column_vals), nrow = length(row_vals), byrow = TRUE)+
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(plot.title = ggplot2::element_text(size = title_size))

  grobs_colnames = list()
  for(i in 1:(length(column_vals))){
    grobs_colnames[[i]] = patchwork::wrap_elements(panel = grid::textGrob(
      column_vals[i], rot = 0,
      gp = grid::gpar(col = "black", fontsize = font_size))
    )
  }

  grobs_rownames = list()
  for(i in 1:(length(row_vals))){
    grobs_rownames[[i]] = patchwork::wrap_elements(panel = grid::textGrob(
      row_vals[i], rot = 90,
      gp = grid::gpar(col = "black", fontsize = font_size))
    )
  }

  column_name_strip = patchwork::wrap_plots(grobs_colnames, nrow = 1)

  final_plot = (column_name_strip / panels_matrix)+
    patchwork::plot_layout(heights = c(1, col_title_ratio))

  row_name_strip = patchwork::wrap_plots(grobs_rownames, ncol = 1)
  row_name_strip = ((ggplot2::ggplot() + ggplot2::theme_void()) / row_name_strip) +
    patchwork::plot_layout(heights = c(1, col_title_ratio))

  final_plot = (row_name_strip | final_plot) +
    patchwork::plot_layout(widths = c(1, row_title_ratio))

  return(final_plot)
}
