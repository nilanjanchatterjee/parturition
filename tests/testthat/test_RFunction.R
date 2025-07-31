source(here("tests/testthat/helper.R"))

test_data <- test_data("input3_move2loc_LatLon.rds")

rFunction(data = test_data, threshold = 6, window = 756)

test_that("function executes with user-passed threshold", {
  actual <- rFunction(data = test_data, threshold = 6, window = 756)
  expected_count <- 3164
  expect_equal(nrow(actual), expected_count)
})


test_that("function executes with default threshold", {
  actual <- rFunction(data = test_data, window = 756)
  expected_count <- 3164
  expect_equal(nrow(actual), expected_count)
})


test_that("function returns nothing and errors with bad window", {
  actual <- rFunction(data = test_data)
  expect_null(actual)
  expect_false(file.exists())
})


test_that("function preserves input track attributes", {
  actual <- rFunction(data = test_data, window = 756)
  actual_track_data_attributes <- sort(names(mt_track_data(actual)))
  expected_track_data_attirbutes <- sort(names(mt_track_data(test_data)))
  expect_equal(actual_track_data_attributes, expected_track_data_attirbutes)
})


test_that("function preserves input track id column", {
  actual <- rFunction(data = test_data, window = 756)
  actual_track_id_column <- attr(actual, "track_id")
  expected_track_id_column <- attr(test_data, "track_id")
  expect_equal(actual_track_id_column, expected_track_id_column)
})



test_that("function doesn't duplicate columns in output", {
  test_data <- test_data("input_issue_20.rds")
  
  actual <- rFunction(data = test_data, threshold = 6, window = 756)
  expect_contains(colnames(mt_track_data(actual)), c("individual_local_identifier"))
  expect_contains(colnames(actual), c("trackID", "individual_local_identifier_year"))
})
