source(here("tests/testthat/helper.R"))

# Test Data Setup ----
test_data_main <- test_data("input3.rds")
test_data_issue20 <- test_data("input_issue_20.rds")

# Core Functionality Tests with Parameter Variations ----
test_that("function executes with various threshold values", {
  # User-defined threshold
  actual1 <- rFunction(data = test_data_main, threshold = 6, window = 756)
  expect_equal(nrow(actual1), 3164)
  expect_s3_class(actual1, "move2")
  
  # Zero threshold
  actual2 <- rFunction(data = test_data_main, threshold = 0, window = 756)
  expect_s3_class(actual2, "move2")
  
  # High threshold
  actual3 <- rFunction(data = test_data_main, threshold = 1000, window = 756)
  expect_s3_class(actual3, "move2")
  
  # Default threshold (NULL)
  actual4 <- rFunction(data = test_data_main, threshold = NULL, window = 756)
  expect_equal(nrow(actual4), 3164)
})

test_that("function handles various window sizes", {
  # Large window (original test)
  actual1 <- rFunction(data = test_data_main, window = 756)
  expect_equal(nrow(actual1), 3164)
  
  # Medium window
  actual2 <- rFunction(data = test_data_main, window = 168)  # 1 week
  expect_s3_class(actual2, "move2")
  
  # Small window
  actual3 <- rFunction(data = test_data_main, window = 72)   # 3 days
  expect_s3_class(actual3, "move2")
  
  # Very small window (function still processes but may filter some data)
  actual4 <- rFunction(data = test_data_main, window = 1)
  expect_s3_class(actual4, "move2")
  expect_true(nrow(actual4) > 0)  # Should return some data
  
  # Very large window (function still processes data)
  actual5 <- rFunction(data = test_data_main, window = 50000)
  expect_s3_class(actual5, "move2")
  expect_true(nrow(actual5) > 0)  # Should return some data
})

test_that("function handles various yaxis_limit values", {
  # Default yaxis_limit
  actual1 <- rFunction(data = test_data_main, window = 756, yaxis_limit = 1000)
  expect_s3_class(actual1, "move2")
  
  # Small yaxis_limit
  actual2 <- rFunction(data = test_data_main, window = 756, yaxis_limit = 100)
  expect_s3_class(actual2, "move2")
  
  # Large yaxis_limit
  actual3 <- rFunction(data = test_data_main, window = 756, yaxis_limit = 10000)
  expect_s3_class(actual3, "move2")
})

test_that("function handles elevation parameter variations", {
  # Without elevation
  actual1 <- rFunction(data = test_data_main, window = 756, include_elevation = FALSE)
  expect_s3_class(actual1, "move2")
  
  # With elevation (may warn if service unavailable)
  expect_no_error({
    actual2 <- rFunction(data = test_data_main, window = 756, include_elevation = TRUE)
    expect_s3_class(actual2, "move2")
  })
})

test_that("function handles events_file parameter variations", {
  # NULL events file
  actual1 <- rFunction(data = test_data_main, window = 756, events_file = NULL)
  expect_s3_class(actual1, "move2")
  
  # Non-existent events file
  expect_warning({
    actual2 <- rFunction(data = test_data_main, window = 756, events_file = "fake_file.csv")
  }, "Could not load known events file")
  expect_s3_class(actual2, "move2")
})

# Data Integrity and Output Tests ----
test_that("function preserves data structure across parameter combinations", {
  # Test multiple parameter combinations
  params_list <- list(
    list(threshold = 6, window = 756, yaxis_limit = 1000),
    list(threshold = NULL, window = 168, yaxis_limit = 500),
    list(threshold = 10, window = 72, yaxis_limit = 2000, include_elevation = FALSE)
  )
  
  for (params in params_list) {
    actual <- do.call(rFunction, c(list(data = test_data_main), params))
    
    # Preserve track attributes
    actual_attrs <- sort(names(mt_track_data(actual)))
    expected_attrs <- sort(names(mt_track_data(test_data_main)))
    expect_equal(actual_attrs, expected_attrs)
    
    # Preserve track ID column
    expect_equal(attr(actual, "track_id"), attr(test_data_main, "track_id"))
    
    # Should have expected columns
    expected_cols <- c("distance", "timediff", "nsd_km", "speed")
    expect_true(all(expected_cols %in% colnames(actual)))
  }
})

test_that("function doesn't duplicate columns with issue20 data", {
  # Test with both datasets and different parameters
  actual1 <- rFunction(data = test_data_issue20, threshold = 6, window = 756)
  expect_contains(colnames(mt_track_data(actual1)), c("individual_local_identifier"))
  expect_contains(colnames(actual1), c("trackID", "individual_local_identifier_year"))
  expect_equal(length(colnames(actual1)), length(unique(colnames(actual1))))
  
  actual2 <- rFunction(data = test_data_issue20, threshold = NULL, window = 168)
  expect_equal(length(colnames(actual2)), length(unique(colnames(actual2))))
})

# File Output Tests ----
test_that("function creates correct output files with different parameters", {
  temp_dir <- tempdir()
  old_env <- Sys.getenv("APP_ARTIFACTS_DIR")
  Sys.setenv(APP_ARTIFACTS_DIR = temp_dir)
  
  # Test with specific threshold
  actual1 <- rFunction(data = test_data_main, threshold = 6, window = 756)
  csv_file1 <- file.path(temp_dir, "parturition_output_756h_threshold_6.csv")
  pdf_file1 <- file.path(temp_dir, "parturition_analysis_756h_threshold_6.pdf")
  expect_true(file.exists(csv_file1))
  expect_true(file.exists(pdf_file1))
  
  # Test with default threshold (average)
  actual2 <- rFunction(data = test_data_main, threshold = NULL, window = 168)
  csv_file2 <- file.path(temp_dir, "parturition_output_168h_threshold_average.csv")
  pdf_file2 <- file.path(temp_dir, "parturition_analysis_168h_threshold_average.pdf")
  expect_true(file.exists(csv_file2))
  expect_true(file.exists(pdf_file2))
  
  # Test with different window
  actual3 <- rFunction(data = test_data_main, threshold = 10, window = 72)
  csv_file3 <- file.path(temp_dir, "parturition_output_72h_threshold_10.csv")
  pdf_file3 <- file.path(temp_dir, "parturition_analysis_72h_threshold_10.pdf")
  expect_true(file.exists(csv_file3))
  expect_true(file.exists(pdf_file3))
  
  # Clean up
  Sys.setenv(APP_ARTIFACTS_DIR = old_env)
  unlink(c(csv_file1, pdf_file1, csv_file2, pdf_file2, csv_file3, pdf_file3))
})

# Edge Cases and Error Handling ----
test_that("function handles edge cases across parameter ranges", {
  # Minimal viable data
  first_track_id <- unique(mt_track_id(test_data_main))[1]
  single_track_data <- test_data_main[mt_track_id(test_data_main) == first_track_id, ]
  
  actual1 <- rFunction(data = single_track_data, threshold = 5, window = 756, yaxis_limit = 500)
  expect_s3_class(actual1, "move2")
  
  # Very few points (should return data with at least original rows)
  minimal_data <- single_track_data[1:5, ]
  actual2 <- rFunction(data = minimal_data, threshold = 1, window = 72, yaxis_limit = 100)
  expect_s3_class(actual2, "move2")
  expect_true(nrow(actual2) >= nrow(minimal_data))
  
  # Missing individual_local_identifier
  test_data_no_id <- test_data_main
  track_attrs <- mt_track_data(test_data_no_id)
  if ("individual_local_identifier" %in% names(track_attrs)) {
    track_attrs <- track_attrs[, !names(track_attrs) %in% "individual_local_identifier"]
    attr(test_data_no_id, "track_data") <- track_attrs
  }
  
  actual3 <- rFunction(data = test_data_no_id, threshold = 8, window = 168)
  expect_s3_class(actual3, "move2")
})

# Parameter Combination Stress Test ----
test_that("function works with extreme parameter combinations", {
  # High threshold, small window, large yaxis
  actual1 <- rFunction(data = test_data_main, threshold = 500, window = 24, yaxis_limit = 50000)
  expect_s3_class(actual1, "move2")
  
  # Low threshold, large window, small yaxis
  actual2 <- rFunction(data = test_data_main, threshold = 0.1, window = 2000, yaxis_limit = 10)
  expect_s3_class(actual2, "move2")
  
  # All optional parameters
  actual3 <- rFunction(data = test_data_main, threshold = 15, window = 336, 
                       events_file = NULL, yaxis_limit = 750, include_elevation = FALSE)
  expect_s3_class(actual3, "move2")
})

# Validation Tests ----
test_that("movement metrics are valid across parameter variations", {
  test_params <- list(
    list(threshold = 5, window = 168),
    list(threshold = NULL, window = 756),
    list(threshold = 20, window = 72)
  )
  
  for (params in test_params) {
    actual <- do.call(rFunction, c(list(data = test_data_main), params))
    
    # All movement metrics should be non-negative
    expect_true(all(actual$speed >= 0, na.rm = TRUE))
    expect_true(all(actual$distance >= 0, na.rm = TRUE))
    expect_true(all(actual$nsd_km >= 0, na.rm = TRUE))
    expect_true(all(actual$timediff > 0, na.rm = TRUE))
    
    # Coordinates should be reasonable (assuming global data)
    expect_true(all(abs(actual$location_long) <= 180, na.rm = TRUE))
    expect_true(all(abs(actual$location_lat) <= 90, na.rm = TRUE))
  }
})

# Cleanup
rm(test_data_main, test_data_issue20)