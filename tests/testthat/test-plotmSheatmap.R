data(clr)
test_that("plotmSheatmap output is a list for plotting a ggplot object", {
  expect_type(plotmSheatmap(results = mSpreviz(results = iterativeClustering(pairedTimes = pairedTimes(data = clr, sequential = TRUE, common = "_0_"), 
                                                                         common = "_"),
                                           times = pairedTimes(data = clr, sequential = TRUE, common = "_0_")),
                        order = "median", times = c("t1_t25", "t25_t26"),
                        label = TRUE), "list")
})
