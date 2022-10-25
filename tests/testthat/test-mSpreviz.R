data(clr)
test_that("mSpreviz return error when arguments differ in length", {
  expect_error(mSpreviz(results = iterativeClustering(pairedTimes = pairedTimes(data = clr, sequential = TRUE, common = "_0_"), 
                                                      common = "_"), 
                        times = pairedTimes(data = clr, sequential = TRUE, common = "_0_")[1]))
})
