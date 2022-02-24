################################################################################
# Testing - .shift helper function
################################################################################

test_that("shift matches unit tests", {

  # Observed data to be subjected to randomizations
  observed <- matrix(c(1, 4, 7,
                       2, 5, 8,
                       3, 6, 9), nrow = 3, ncol = 3)

  # Move down one row
  realised <- .shift(observed, rowz = 1, colz = 0)
  expected <- matrix(c(7, 1, 4,
                       8, 2, 5,
                       9, 3, 6), nrow = 3, ncol = 3)
  expect_equal(realised, expected, tolerance = 0.001)

  # Move up one row
  realised <- .shift(observed, rowz = -1, colz = 0)
  expected <- matrix(c(4, 7, 1,
                       5, 8, 2,
                       6, 9, 3), nrow = 3, ncol = 3)
  expect_equal(realised, expected, tolerance = 0.001)

  # Move one column right
  realised <- .shift(observed, rowz = 0, colz = 1)
  expected <- matrix(c(3, 6, 9,
                       1, 4, 7,
                       2, 5, 8), nrow = 3, ncol = 3)
  expect_equal(realised, expected, tolerance = 0.001)

  # Move one column left
  realised <- .shift(observed, rowz = 0, colz = -1)
  expected <- matrix(c(2, 5, 8,
                       3, 6, 9,
                       1, 4, 7), nrow = 3, ncol = 3)
  expect_equal(realised, expected, tolerance = 0.001)

  # Move one column right and one row down
  realised <- .shift(observed, rowz = 1, colz = 1)
  expected <- matrix(c(9, 3, 6,
                       7, 1, 4,
                       8, 2, 5), nrow = 3, ncol = 3)
  expect_equal(realised, expected, tolerance = 0.001)

})
