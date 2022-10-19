test_that("mSlineCV output is a list for plotting a ggplot object", {
  expect_type(mSlinesCV(pairedTimes(data = clr, sequential = TRUE, common = "_0_")$t1_t25,
                        iterativeClusteringCV(pairedTimes = pairedTimes(data = clr, sequential = TRUE, common = "_0_"),
                                              results = iterativeClustering(pairedTimes = pairedTimes(data = clr, sequential = TRUE, common = "_0_"),
                                                                            common = "_"), name = "t1_t25",
                                              common = "_", k = 2L),
                        k = 2L), "list")
})
