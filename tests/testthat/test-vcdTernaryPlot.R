test_that("Test vcdTernaryPlot", {
  data_for_ternary <- data.frame(Basal = c(1,2,2,0,0,1,0,1,0),
                                 ML    = c(0,1,0,1,2,2,0,0,1),
                                 LP    = c(0,0,1,0,1,0,1,2,2) )

  expect_silent(vcdTernaryPlot(data = data_for_ternary))

  expect_silent(vcdTernaryPlot(data = data_for_ternary,
                 group = rep(c("Bas","ML","LP"),each=3),
                 group_levels = c("Bas","LP","ML"),
                 group_color = c("red","green","blue"),
                 point_size = 1,
                 legend_point_size = 0.6))

  expect_silent(vcdTernaryPlot(data = data_for_ternary,
                               group = rep(c("Bas","ML","LP"),each=3),
                               group_levels = c("Bas","LP","ML"),
                               group_color = c("red","green","blue"),
                               point_size = 1,
                               facet = TRUE,
                               legend_point_size = 0.6))
})
