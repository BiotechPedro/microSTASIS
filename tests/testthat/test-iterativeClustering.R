data(clr)
test_that("iterativeClustering return a list with the same length as the input", {
  expect_equal(length(pairedTimes(data = clr, sequential = TRUE, common = "_0_")),
               length(iterativeClustering(pairedTimes = pairedTimes(data = clr, sequential = TRUE, common = "_0_"),
                                          common = "_")))
})
