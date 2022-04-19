#' Generate layout matrix for gridExtra::grid.arrange
#'
#' @param grobs_list A list of graphical objects (grobs)
#' @param last_is_legend Whether last graphical object is legend
#' @param scale_legend Scaled width of legend
#'
#' @export
#'
#' @examples
#' prepare_layout_matrix( grobs_list = list(A=1,B=2,C=3) )
#'
#' prepare_layout_matrix( grobs_list = 1:3,
#'                        last_is_legend = FALSE )
#'

prepare_layout_matrix = function(grobs_list = NULL,
                                 last_is_legend = TRUE,
                                 scale_legend = 1){

  ### combine plots using grid.arrange()
  n_grobs = length(grobs_list)

  # if the last one is legend
  if(last_is_legend) {
    n_grobs = n_grobs - 1
  }

  my_ncol = ceiling(sqrt(n_grobs))
  my_nrow = ifelse(my_ncol*(my_ncol-1) >= n_grobs,
                   (my_ncol-1),my_ncol)

  # add one more column for legend
  if(last_is_legend){
    my_width = c(rep(2,my_ncol),1*scale_legend)

  }else{
    my_width = rep(2,my_ncol)
  }

  my_height = rep(2,my_nrow)

  my_layout_mat = matrix(NA,
                         nrow = my_nrow,
                         ncol = ifelse(last_is_legend,my_ncol+1, my_ncol))

  # order of plots by row
  my_order_plots = as.vector(my_layout_mat[,c(1:my_ncol)])
  my_order_plots[1:n_grobs] = c(1:n_grobs)
  my_layout_mat[,(1:my_ncol)] = matrix(my_order_plots,
                                       ncol = my_ncol,byrow = TRUE)
  # last column for legend
  if(last_is_legend){
    my_layout_mat[,(my_ncol + 1 )] = length(grobs_list)
  }

  # final column number, if the last column if legend, add 1
  my_ncol_final = ifelse(last_is_legend,(my_ncol + 1),my_ncol)

  list(ncol = my_ncol_final,
       widths = my_width,
       heights = my_height,
       layout_matrix = my_layout_mat)
}
