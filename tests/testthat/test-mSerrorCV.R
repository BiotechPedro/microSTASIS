data(clr)
test_that("mSerrorCV return a numeric with the MAE values", {
  expect_type(mSerrorCV(pairedTimes(data = clr, sequential = TRUE, common = "_0_")$t1_t25,
                         iterativeClusteringCV(pairedTimes = pairedTimes(data = clr, sequential = TRUE, common = "_0_"),
                                               results = iterativeClustering(pairedTimes = pairedTimes(data = clr, sequential = TRUE, common = "_0_"),
                                                                             common = "_"), name = "t1_t25",
                                               common = "_", k = 2L),
                         k = 2L), "double")
})
