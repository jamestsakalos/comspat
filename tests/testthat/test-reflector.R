################################################################################
# Testing - .reflector( helper function
################################################################################

test_that(".reflector matches unit tests", {

  # Observed data to be subjected to randomizations
  observed <- matrix(c(0, 1, 1,
                       0, 0, 1,
                       0, 0, 0), nrow = 3, ncol = 3)

  # Not doing anything
  realised <- .reflector(observed, control = "None")
  expect_equal(realised, observed, tolerance = 0.001)


  # Reflecting columns
  realised <- .reflector(observed, control = "colz")
  expected <- matrix(c(0, 0, 0,
                       0, 0, 1,
                       0, 1, 1), nrow = 3, ncol = 3)
  expect_equal(realised, expected, tolerance = 0.001)

  # Reflecting rows
  realised <- .reflector(observed, control = "rowz")
  expected <- matrix(c(1, 1, 0,
                       1, 0, 0,
                       0, 0, 0), nrow = 3, ncol = 3)
  expect_equal(realised, expected, tolerance = 0.001)

  # Reflecting rows and columns
  realised <- .reflector(observed, control = "colz&rowz")
  expected <- matrix(c(0, 0, 0,
                       1, 0, 0,
                       1, 1, 0), nrow = 3, ncol = 3)
  expect_equal(realised, expected, tolerance = 0.001)

})
