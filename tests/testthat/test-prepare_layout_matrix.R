test_that("Test prepare_layout_matrix", {

  expect_equal(prepare_layout_matrix( grobs_list = list(A=1,B=2,C=3) ),
               prepare_layout_matrix( grobs_list = 1:3 ) )

  expect_silent(prepare_layout_matrix(grobs_list = 1:3,
                                      last_is_legend = FALSE))

  expect_error(prepare_layout_matrix(1))

  expect_silent(prepare_layout_matrix(1,last_is_legend = FALSE))
})
