data(clr)
test_that("mSmetadataGroups return a character", {
  expect_type(mSmetadataGroups(metadata = data.frame(Sample = rownames(clr), age = c(rep("youth", 65), rep("old", 131-65))), 
                                samples = rownames(clr), common = "_",
                                individuals = mSpreviz(results = iterativeClustering(pairedTimes = pairedTimes(data = clr, sequential = TRUE, common = "_0_"), 
                                                                                     common = "_"),
                                                       times = pairedTimes(data = clr, sequential = TRUE, common = "_0_"))$individual, 
                                variable = "age"), "character")
})
