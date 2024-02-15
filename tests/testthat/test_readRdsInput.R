#this test is internal only to check if the move1-move2 double read-in works

library('move2')

source("../../src/io/rds.R")

test_that("read move2", {
  actual <- readRdsInput(sourceFile = "data/input3_move2loc_LatLon.rds")
  expect_true(mt_is_move2(actual))
})
