
#' Generate data for ternary plot
#'
#' @description Generate a three column matrix which can be used to draw a ternary plot.
#'
#' @param data_exp_mat An expression matrix, e.g., raw count matrix or log2CPM matrix
#' @param anno_signature_genes A data.frame containing signature gene annotation
#' @param gene_name_col Colname name of row (gene) names used in the expression matrix
#' @param gene_type_col Colname name of signature gene type annotation
#' @param weight_by_gene_count Whether to divide the signature gene number
#'                             by the total signature gene name,
#'                             default is TRUE
#' @param print_gene_num Print the signature gene number or not
#' @param cutoff_exp Expression cut-offs used to count the signature gene number
#' @param prior_count Add a prior count to avoid signature gene number to be 0,
#'                    default is 2 but can be set to a different one
#'
#'
#' @export
#'
#' @examples
#' library(scTernary)
#'
#' # Generate data for ternary plots
#' # using a CPM matrix and a CPM cut-off of 100
#'
#' data_for_ternary <- generate_data_for_ternary(
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
#' vcdTernaryPlot(data = data_for_ternary,
#'   order_colnames = c(2,3,1),
#'   group = example_dge_data$samples$group,
#'   group_color = c("red","green","blue"),
#'   point_size = 1,
#'   legend_point_size = 0.6,
#'   legend_position = c(0.3,0.5),
#'   scale_legend = 1)
#'

generate_data_for_ternary <- function(data_exp_mat = NULL,
                                      anno_signature_genes = NULL,
                                      gene_name_col = "GeneID",
                                      gene_type_col = "gene_type",
                                      weight_by_gene_count = TRUE,
                                      print_gene_num = FALSE,
                                      cutoff_exp = 0,
                                      prior_count = 2){


  name_signatures <- levels( factor(anno_signature_genes[,gene_type_col]) )

  # prior count for signatures
  if(length(prior_count)==1){
    prior_count <- rep(prior_count, length(name_signatures))
    names(prior_count) <- name_signatures
  }else if(is.null(names(prior_count) ) ){
    names(prior_count) <- name_signatures
  }

  data_for_ternary <- matrix(0L,
                             nrow = ncol(data_exp_mat),
                             ncol = length(name_signatures))

  rownames(data_for_ternary) <- colnames(data_exp_mat)
  colnames(data_for_ternary) <- c(name_signatures)

  for (i in name_signatures ) {
    # get geneID or symbol of one signature (Basal, LP or ML)
    genes_of_one_signature <- subset(anno_signature_genes,
                                     get(gene_type_col) == i)[,gene_name_col]

    # common genes between genes of one signature and genes in expression matrix
    genes_of_one_signature <- intersect(genes_of_one_signature, rownames(data_exp_mat) )

    assign(i, genes_of_one_signature)

    # cutoff_exp can be one single value or a vector with length of ncol(data_exp_mat)
    data_for_ternary[, i] <- rowSums(t(data_exp_mat[get(i),]) > cutoff_exp)

    # add a prior count
    data_for_ternary[, i] <- data_for_ternary[, i] + prior_count[i]

    # whether divide expressed gene count by common gene count used
    if (weight_by_gene_count) {
      if(print_gene_num) {print( paste0(i,": " ,length(genes_of_one_signature) ))}
      data_for_ternary[, i] <- data_for_ternary[, i] / length(genes_of_one_signature)
    }
  }

  data_for_ternary
}


