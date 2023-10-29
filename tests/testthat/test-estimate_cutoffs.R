test_that("Test estimate_optimized_cutoffs", {

  lcpm <- edgeR::cpm(example_dge_data$counts,log = TRUE)

  estimated_cutoffs <- estimate_optimized_cutoffs(
    data_exp_mat = lcpm,
    anno_signature_genes = anno_signature_genes_mouse,
    gene_name_col = "GeneID",
    gene_type_col = "gene_type",
    weight_by_gene_count = TRUE,
    prior_count = 2,
    do_parallel = TRUE,
    n_cores = 2
  )

  expect_equal(length(estimated_cutoffs),ncol(lcpm))

  expect_false(NA %in% estimated_cutoffs )

})
