test_that("mSinternalPairedTimes generated the correct names for the list of pairedTimes", {
  expect_match(names(mSinternalPairedTimes(data = clr, specifiedTimePoints = c("1", "25"), common = "_0_")),
               "t[[:digit:]]_t")
  expect_match(names(mSinternalPairedTimes(data = clr, specifiedTimePoints = c("25", "26"), common = "_0_")),
               "t[[:digit:]][[:digit:]]_t")
})
