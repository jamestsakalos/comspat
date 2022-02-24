################################################################################
# Testing - .shift_transect helper function
################################################################################

test_that(".shift_transect matches unit tests", {

  data <- data.frame("X" = c(1:3, 4:6), "Species" = c(rep("A", 3),
                                                      rep("B", 3)))

  # Do nothing
  realised <- .shift_transect(data, dim_max = 6, reflection = "3",
                              shift = "3", control = NULL)
  expect_equal(realised, data, tolerance = 0.001)

  # Reflect the data
  realised <- .shift_transect(data, dim_max = 6, reflection = "2", shift = "3",
                              control = NULL)
  expected <- data.frame("X" = c(1:3, 4:6), "Species" = c(rep("B", 3),
                                                          rep("A", 3)))
  expect_equal(realised, expected, tolerance = 0.001)

  # Shift the data
  realised <- .shift_transect(data, dim_max = 6, reflection = "3",
                              shift = "2", control = 1)
  expected <- data.frame("X" = c(1:3, 4:6), "Species" = c(rep("A", 2),
                                                          rep("B", 3),
                                                          "A"))
  expect_equal(realised, expected, tolerance = 0.001)

  # Shift and reflect the data
  realised <- .shift_transect(data, dim_max = 6, reflection = "2",
                              shift = "2", control = 1)
  expected <- data.frame("X" = c(1:3, 4:6), "Species" = c(rep("B", 2),
                                                          rep("A", 3),
                                                          "B"))
  expect_equal(realised, expected, tolerance = 0.001)

})
