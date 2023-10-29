#' Draw a ternary plot
#'
#' @description vcdTernaryPlot() is modified from vcd::ternaryplot()
#' and can be used to draw a ternary plot.
#'
#' @inheritParams prepare_layout_matrix
#' @param data A data.frame or matrix of three columns
#' @param order_colnames A vector to order the three columns
#' @param group A vector to specify row (cell) annotation or classification
#' @param group_levels Levels of the group vector, default is levels(factor(group))
#' @param group_color Colors used for the group levels
#' @param label_position_x X positions for the three labels
#' @param label_position_y Y positions for the three labels
#' @param show_legend Whether to show the legend
#' @param legend_position Legend position, e.g., 'right' or 'center' or c(0.1,0.5)
#' @param prop_size Same parameter used in vcd::ternaryplot(),
#' if TRUE, the symbol size is plotted proportional to the row sum of the three variables
#' @param point_shape Shape for the point, default is pch 16
#' @param point_size Size of point, default is 0.5
#' @param label_text_size Size of the three labels, default is 1.2
#' @param legend_point_shape Shape for the point of legend, default is pch 16
#' @param legend_point_size Size of point of legend, default is 0.5
#' @param legend_text_size Text size of legend, default is 1.5
#' @param legend_vertical_space Vertical space between legend texts, default is 1.5 (lines)
#' @param facet whether to show facets
#' @param title Title of the ternary plot, default is NULL
#'
#' @rdname vcdTernaryPlot_functions
#' @export
#'
#' @examples
#' data_for_ternary <- data.frame(Basal = c(1,2,2,0,0,1,0,1,0),
#'                               ML    = c(0,1,0,1,2,2,0,0,1),
#'                               LP    = c(0,0,1,0,1,0,1,2,2) )
#'
#' vcdTernaryPlot(data = data_for_ternary,
#'                group = rep(c("Bas","ML","LP"),each=3),
#'                group_levels = c("Bas","LP","ML"),
#'                group_color = c("red","green","blue"),
#'                point_size = 1,
#'                legend_point_size = 0.6)

vcdTernaryPlot <- function(data = NULL,
                           order_colnames = c(3,2,1),
                           group = NULL,
                           group_levels = levels(factor(group)),
                           group_color = NULL,
                           label_position_x = c(0.1,0.9,0.5),
                           label_position_y = c(0.05,0.05,0.85),
                           show_legend = FALSE,
                           legend_position = c(0.1,0.5),
                           prop_size = FALSE,
                           point_shape = 16,
                           point_size = 0.5,
                           label_text_size = 1.2,
                           legend_point_shape = 16,
                           legend_point_size  = 0.5,
                           legend_text_size = 1.5,
                           legend_vertical_space = 1.5,
                           scale_legend = 0.5,
                           n_col = NULL,
                           facet=FALSE,
                           title = NULL){

  # if colors are not set, use the default colors
  if(is.null(group_color)){
    group_color <- c("#6495ED", "#BF3EFF", "#FF3030", "#FFD700", "#ADFF2F",
                     "#00FA9A", "#48D1CC", "#FFA500", "#FFC0CB", "#CD1076",
                     "#EE82EE", "#FF00FF", "#8B6914", "#00FFFF", "#E5E5E5")
  }

  group_color <- group_color[1:length(group_levels)]

  # if group is null, not show legend
  if(is.null(group)){
    show_legend <- FALSE
    facet <- FALSE}

  # set color for each group
  if(!is.null(group)){
    my_color <- factor(group,
                       labels = group_color,
                       levels = group_levels)

    my_color <- as.character(my_color)  # should not be factor

  }else{
    my_color <- "red"
  }

  my_labels_axis <- colnames(data)[order_colnames]

  grobs_list <- list()

  if(facet==FALSE){
    i <- 1
    grobs_list[[i]] <- grid::grid.grabExpr(
      {
        # draw ternary plot
        vcd::ternaryplot(data[,my_labels_axis],
                         dimnames = "",
                         col = my_color, # should not be factor
                         prop_size = prop_size,
                         main = title,
                         pch  = point_shape,
                         cex  = point_size)

        # add label to axis
        grid::grid.text(my_labels_axis[1],
                        x = label_position_x[1],
                        y = label_position_y[1],
                        gp = grid::gpar(cex=label_text_size,fontface="bold") )

        grid::grid.text(my_labels_axis[2],
                        x = label_position_x[2],
                        y = label_position_y[2],
                        gp = grid::gpar(cex=label_text_size,fontface="bold") )

        grid::grid.text(my_labels_axis[3],
                        x =label_position_x[3],
                        y = label_position_y[3],
                        gp = grid::gpar(cex=label_text_size,fontface="bold") )
      }
      ,warn = 0)

  }else{
    for (i in 1:length(group_levels) ) {
      grobs_list[[i]] <- grid::grid.grabExpr(
        {
          # draw ternary plot
          vcd::ternaryplot(data[group==group_levels[i],my_labels_axis],
                           dimnames = "",
                           col = my_color[my_color==group_color[i]], # should not be factor
                           prop_size = prop_size,
                           main = title,
                           pch  = point_shape,
                           cex  = point_size)

          # add label to axis
          grid::grid.text(my_labels_axis[1],
                          x = label_position_x[1],
                          y = label_position_y[1],
                          gp = grid::gpar(cex=label_text_size,fontface="bold") )

          grid::grid.text(my_labels_axis[2],
                          x = label_position_x[2],
                          y = label_position_y[2],
                          gp = grid::gpar(cex=label_text_size,fontface="bold") )

          grid::grid.text(my_labels_axis[3],
                          x =label_position_x[3],
                          y = label_position_y[3],
                          gp = grid::gpar(cex=label_text_size,fontface="bold") )
        }
        ,warn = 0)

    }

  }

  # add legend
  if( show_legend ){
    grobs_list[[i+1]] <- grid::grid.grabExpr(vcd::grid_legend(legend_position,
                                                              pch = point_shape,
                                                              size = legend_point_size,
                                                              col = group_color,
                                                              labels = group_levels,
                                                              gp = grid::gpar(cex=legend_text_size),
                                                              vgap = grid::unit(legend_vertical_space, "lines"),
                                                              frame = FALSE,
                                                              title = NULL)
                                             ,warn = 0)
  }


  my_layout_matrix <- prepare_layout_matrix(grobs_list,
                                            last_is_legend = show_legend,
                                            n_col = n_col,
                                            scale_legend   = scale_legend)

  gridExtra::grid.arrange( grobs = grobs_list,
                           ncol= my_layout_matrix[["ncol"]],
                           widths = my_layout_matrix[["widths"]],
                           heights = my_layout_matrix[["heights"]],
                           respect = TRUE,
                           layout_matrix = my_layout_matrix[["layout_matrix"]] )

}

