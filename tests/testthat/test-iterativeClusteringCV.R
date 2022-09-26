test_that("iterativeClusteringCV return error when dividing individuals by k doesn't result in integer", {
  expect_error(iterativeClusteringCV(pairedTimes = pairedTimes(data = clr, sequential = TRUE, common = "_0_"),
                                     results = iterativeClustering(pairedTimes = pairedTimes(data = clr, sequential = TRUE, common = "_0_"),
                                                                   parallel = TRUE, common = "_"), name = "t1_t25",
                                     common = "_", k = 3L, parallel = TRUE))
})

test_that("iterativeClusteringCV return a list length equal to the number of individuals in each paired time", {
  expect_equal(dim(pairedTimes(data = clr, sequential = TRUE, common = "_0_")[[1]])[1] / 2,
               length(iterativeClusteringCV(pairedTimes = pairedTimes(data = clr, sequential = TRUE, common = "_0_"),
                                            results = iterativeClustering(pairedTimes = pairedTimes(data = clr, sequential = TRUE, common = "_0_"),
                                                                          parallel = TRUE, common = "_"), name = "t1_t25",
                                            common = "_", k = 2L, parallel = TRUE)))
})
