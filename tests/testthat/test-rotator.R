################################################################################
# Testing - .rotator helper function
################################################################################

test_that(".rotator matches unit tests", {

  # Observed data to be subjected to randomizations
  observed <- matrix(c(1, 0, 0,
                       1, 0, 0,
                       1, 0, 0), nrow = 3, ncol = 3)

  # No rotations
  realised <- .rotator(observed, control = 1)
  expect_equal(realised, observed, tolerance = 0.001)

  # 90 degrees
  realised <- .rotator(observed, control = 2)
  expected <- matrix(c(0, 0, 0,
                       0, 0, 0,
                       1, 1, 1), nrow = 3, ncol = 3)
  expect_equal(realised, expected, tolerance = 0.001)

  # 180 degrees
  realised <- .rotator(observed, control = 3)
  expected <- matrix(c(0, 0, 1,
                       0, 0, 1,
                       0, 0, 1), nrow = 3, ncol = 3)
  expect_equal(realised, expected, tolerance = 0.001)

  # 270 degrees
  realised <- .rotator(observed, control = 4)
  expected <- matrix(c(1, 1, 1,
                       0, 0, 0,
                       0, 0, 0), nrow = 3, ncol = 3)
  expect_equal(realised, expected, tolerance = 0.001)

})
