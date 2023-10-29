
#' Calculate the coefficient of variation (CV)
#'
#' @description Calculate the coefficient of variation (CV) of each row of
#' the result matrix from generate_data_for_ternary().
#' calculate_cv_by_row() is not used separately but used in estimate_optimized_cutoffs()
#' to obtain the optimized cut-off.
#' @inheritParams generate_data_for_ternary
#' @param ... Other parameters in generate_data_for_ternary()
#'
#' @export
#'
#' @examples
#'
#' calculate_cv_by_row(
#'   data_exp_mat = edgeR::cpm(example_dge_data$counts,
#'                             log = FALSE),
#'   anno_signature_genes = anno_signature_genes_mouse,
#'   gene_name_col = "GeneID",
#'   gene_type_col = "gene_type",
#'   weight_by_gene_count = FALSE,
#'   cutoff_exp = 100,
#'   prior_count = 0
#' )
#'
#'
calculate_cv_by_row <- function(cutoff_exp,...){

  data_for_ternary <- generate_data_for_ternary(cutoff_exp = cutoff_exp,
                                                ...)

  # Coefficient of variation, if mean is 0, set CV to 0
  f_cv <- function(x){ ifelse(mean(x)==0,0,stats::sd(x)/mean(x) ) }

  # Coefficient of variation
  cv_by_row <- apply(data_for_ternary, 1, f_cv)

  cv_by_row
}


#' Estimate the optimized cut-off
#'
#' @description Estimate the optimized cut-off that maximizes the
#' coefficient of variation (CV) of each cell or sample.
#' @inheritParams generate_data_for_ternary
#' @param interval A sequence of cut-offs used for calculating the CVs,
#'                 and the cut-off that maximize the CV is the optimized cut-off
#' @param do_parallel Whether do parallel computation or not,
#'                    logical value, default is TRUE
#' @param n_cores Number of cores used for parallel computation,
#'                half of the total cores will be used if not provided
#' @export
#'
#' @examples
#' # Set 'weight_by_gene_count' to TRUE is recommended for estimating the optimized cut-offs
#'
#' start_time <- proc.time()
#'
#' estimated_cutoffs <- estimate_optimized_cutoffs(
#'   data_exp_mat = edgeR::cpm(example_dge_data$counts,
#'                             log = TRUE),
#'   anno_signature_genes = anno_signature_genes_mouse,
#'   gene_name_col = "GeneID",
#'   gene_type_col = "gene_type",
#'   weight_by_gene_count = TRUE,
#'   prior_count = 2,
#'   do_parallel = TRUE,
#'   n_cores = 2
#' )
#'
#' end_time <- proc.time() - start_time
#'
#' end_time[3]
#'
#' estimated_cutoffs
#'
#' data_for_ternary <- generate_data_for_ternary(
#'   data_exp_mat = edgeR::cpm(example_dge_data$counts,
#'                             log = TRUE),
#'   anno_signature_genes = anno_signature_genes_mouse,
#'   gene_name_col = "GeneID",
#'   gene_type_col = "gene_type",
#'   weight_by_gene_count = TRUE,
#'   cutoff_exp = estimated_cutoffs,
#'   prior_count = 2
#' )
#'
#' vcdTernaryPlot(data = data_for_ternary,
#'   order_colnames = c(2,3,1),
#'   group = example_dge_data$samples$group,
#'   group_color = c("red","green","blue"),
#'   point_size = 1,
#'   legend_point_size = 0.6,
#'   legend_position = c(0.3,0.5),
#'   scale_legend = 1)
#'
#'
estimate_optimized_cutoffs <- function(data_exp_mat = NULL,
                                       interval = seq(from = floor(min(data_exp_mat)),
                                                      to = ceiling(max(data_exp_mat)),
                                                      length.out = 1000),
                                       gene_name_col = "GeneID",
                                       gene_type_col = "gene_type",
                                       anno_signature_genes = NULL,
                                       weight_by_gene_count = TRUE,
                                       prior_count = 2,
                                       do_parallel = TRUE,
                                       n_cores = NULL){

  data_exp_mat <- as.data.frame(data_exp_mat)

  if(do_parallel == TRUE){

    # do parallel
    if( is.null(n_cores) || !(is.numeric(n_cores)) ){
      n_cores <- parallel::detectCores(logical = TRUE)/2
    }

    cl <- parallel::makeCluster(n_cores)

    parallel::clusterExport(cl, deparse(substitute(calculate_cv_by_row)))

    res <- parallel::parSapply(cl,
                               interval,
                               FUN = calculate_cv_by_row,
                               data_exp_mat =  data_exp_mat,
                               gene_name_col = gene_name_col,
                               gene_type_col = gene_type_col,
                               anno_signature_genes = anno_signature_genes,
                               weight_by_gene_count = weight_by_gene_count,
                               prior_count = prior_count)
    parallel::stopCluster(cl)

  }else{
    # not do parallel
    res <- sapply(interval,
                  FUN = calculate_cv_by_row,
                  data_exp_mat =  data_exp_mat,
                  gene_name_col = gene_name_col,
                  gene_type_col = gene_type_col,
                  anno_signature_genes = anno_signature_genes,
                  weight_by_gene_count = weight_by_gene_count,
                  prior_count = prior_count)

  }

  # if only one cell,  transpose res (row is cell, columns are cutoffs)
  if(ncol(data_exp_mat)==1){res <- t(res);rownames(res) <- colnames(data_exp_mat)}

  # return the cut-off with maximum CV
  res <- apply(res, 1, function(x){n <- which.max(x); interval[n]} )
  res
}
