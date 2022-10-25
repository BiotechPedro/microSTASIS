data(clr)
test_that("pairedTimes return error when any of specifiedTimePoints doesn't exist", {
  expect_error(pairedTimes(data = clr, sequential = FALSE, common = "_0_",
                           specifiedTimePoints = c("1", "2")))
  expect_error(pairedTimes(data = both_tse, sequential = FALSE, assay = "counts",
                           alternativeExp = NULL, ID = "ID", timePoints = "time_point",
                           specifiedTimePoints = c("1", "2")))
  expect_error(pairedTimes(data = both_tse, sequential = FALSE, assay = "data",
                           alternativeExp = "unnamed1", ID = "ID", timePoints = "time_point",
                           specifiedTimePoints = c("1", "2")))
})

test_that("pairedTimes return error when assay or alternative experiment names are incorrect", {
  expect_error(pairedTimes(data = both_tse, sequential = FALSE, assay = "countss",
                           alternativeExp = NULL, ID = "ID", timePoints = "time_point",
                           specifiedTimePoints = c("1", "2")))
  expect_error(pairedTimes(data = both_tse, sequential = FALSE, assay = "data_error",
                           alternativeExp = "unnamed1", ID = "ID", timePoints = "time_point",
                           specifiedTimePoints = c("1", "2")))
  expect_error(pairedTimes(data = both_tse, sequential = FALSE, assay = "data",
                           alternativeExp = "named0", ID = "ID", timePoints = "time_point",
                           specifiedTimePoints = c("1", "2")))
})

test_that("pairedTimes return error when ID or timePoints not in colnames(colData(data))", {
  expect_error(pairedTimes(data = both_tse, sequential = FALSE, assay = "counts",
                           alternativeExp = NULL, ID = "id", timePoints = "time_point",
                           specifiedTimePoints = c("1", "2")))
  expect_error(pairedTimes(data = both_tse, sequential = FALSE, assay = "data",
                           alternativeExp = "unnamed1", ID = "ID", timePoints = "time_points",
                           specifiedTimePoints = c("1", "2")))
})


