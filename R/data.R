#' Annotation of signature genes of different cell types of mouse mammary gland.
#'
#' A dataset containing 1470 Basal genes, 428 LP genes and 528 ML genes
#'
#' @format A data frame with 2427 rows and 4 columns:
#' \describe{
#'   \item{GeneID}{Gene ID from NCBI}
#'   \item{Symbol}{Gene symbol}
#'   \item{Chr}{Chromosome location}
#'   \item{gene_type}{Signature gene type}
#' }
#' @source \url{https://breast-cancer-research.biomedcentral.com/articles/10.1186/s13058-021-01445-4}
"anno_signature_genes_mouse"


#' An example dataset used in the example codes.
#'
#' A DGElist object containing 3 Basal, 3 LP and 3 ML bulk RNA-seq samples
#'
#' @format A DGElist object with raw count matrix, sample annotation data.frame and gene annotation data.frame:
#' \describe{
#'   \item{counts}{raw count matrix}
#'   \item{samples}{Sample annotation}
#'   \item{genes}{Gene annotation}
#' }
#' @source \url{https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE63310}
"example_dge_data"
