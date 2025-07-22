## facet_grid but for our sweet gam fits
##  will also handle
##    (a) missing panels (e.g., we had not data for a given combination of factor predictors)
##    (b) subpanels: multiple terms in the panels_df that match a single combination of column + row variables.
#' Title
#'
#' @param panels_df A dataframe from plot_seasonal_gam_panels.
#' @param column_var 1 dimensional character vector of variable defining the columns of the grid. Must be factor variable present in `panels_df`.
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
    column_var, row_var,
    panel_titles = FALSE,
    col_title_ratio = 30,
    row_title_ratio = 30,
    font_size = 18,
    new_y_label = NULL,
    new_x_label = NULL,
    title_size = 8

){


  row_vals = sort(unique(panels_df[[row_var]]))
  column_vals = sort(unique(panels_df[[column_var]]))

  list_figs = rep(list(ggplot2::ggplot() + ggplot2::theme_void()),
                  length(row_vals) * length(column_vals))

  # panels_matrix = wrap_plots(list_figs,
  #                            ncol = length(column_vals), nrow = length(row_vals), byrow = TRUE)

  ##
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
      ## janky title handling. Consider complex story with multiple miniplots
      # if(nrow(cur.plot) == 1 & !panel_titles){
      #   ## overwrite lone wolf titles
      #   ## structure of subpanels is tricky: could be patchwork with histogram on bottom,
      #   ## or just ggplot
      #   ##
      #   if("patchwork" %in% class(list_figs[[1]][[1]])){
      #     list_figs[[ind]][[1]][[1]] = list_figs[[ind]][[1]][[1]] + ggtitle("")
      #   } else {
      #     list_figs[[ind]][[1]] = list_figs[[ind]][[1]] + ggtitle("")
      #   }
      # }

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
