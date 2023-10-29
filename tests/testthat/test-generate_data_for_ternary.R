test_that("Test generate_data_for_ternary", {
  data_for_ternary <- generate_data_for_ternary(
    data_exp_mat = edgeR::cpm(example_dge_data$counts,
                              log = FALSE),
    anno_signature_genes = anno_signature_genes_mouse,
    gene_name_col = "GeneID",
    gene_type_col = "gene_type",
    weight_by_gene_count = FALSE,
    cutoff_exp = 100,
    prior_count = 0
  )

  expect_equal(ncol(data_for_ternary),3)

  expect_false(NA %in% data_for_ternary)

})
