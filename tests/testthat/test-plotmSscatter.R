data(clr)
test_that("plotmSscater output is a list for plotting a ggplot with ggside extension", {
  expect_type(plotmSscatter(results = mSpreviz(results = iterativeClustering(pairedTimes = pairedTimes(data = clr, sequential = TRUE, common = "_0_"), 
                                                                       common = "_"),
                                         times = pairedTimes(data = clr, sequential = TRUE, common = "_0_")),
                      order = "median", times = c("t1_t25", "t25_t26"),
                      gridLines = TRUE, sideScale = 0.2), "list")
})
