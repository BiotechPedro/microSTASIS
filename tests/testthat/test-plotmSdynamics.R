data(clr)
test_that("plotmSdynamics output is a list for plotting a ggplot object", {
  expect_type(plotmSdynamics(results = mSpreviz(results = iterativeClustering(pairedTimes = pairedTimes(data = clr, sequential = TRUE, common = "_0_"), 
                                                                          common = "_"),
                                            times = pairedTimes(data = clr, sequential = TRUE, common = "_0_")), 
                         groups = mSmetadataGroups(metadata = data.frame(Sample = rownames(clr), age = c(rep("youth", 65), rep("old", 131-65))), 
                                                   samples = rownames(clr), common = "_",
                                                   individuals = mSpreviz(results = iterativeClustering(pairedTimes = pairedTimes(data = clr, sequential = TRUE, common = "_0_"), 
                                                                                                        common = "_"),
                                                                          times = pairedTimes(data = clr, sequential = TRUE, common = "_0_"))$individual, 
                                                   variable = "age"), points = TRUE, linetype = 0),
              "list")
})
