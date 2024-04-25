source(here("tests/testthat/helper.R"))

test_data <- test_data("input3_move2.rds")


test_that("function executes with user-passed threshold", {
  actual <- rFunction(data = test_data, threshold = 72)
  expected_count <- 3168
  expect_equal(nrow(actual), expected_count)
})


test_that("function executes with default threshold", {
  actual <- rFunction(data = test_data)
  expected_count <- 3168
  expect_equal(nrow(actual), expected_count)
})


test_that("function preserves input track attributes", {
  actual <- rFunction(data = test_data)
  actual_track_data_attributes <- names(mt_track_data(actual))
  expected_track_data_attirbutes <- names(mt_track_data(test_data))
  expect_equal(actual_track_data_attributes, expected_track_data_attirbutes)
})


test_that("function preserves input track id column", {
  actual <- rFunction(data = test_data)
  actual_track_id_column <- attr(actual, "track_id")
  expected_track_id_column <- attr(test_data, "track_id")
  expect_equal(actual_track_id_column, expected_track_id_column)
})