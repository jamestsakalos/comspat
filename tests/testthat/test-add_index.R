################################################################################
# Testing - .add_index helper function
################################################################################

test_that("Add the unique SU number to grid data", {

  temp_index <- t(matrix(data = 1:(3 ^ 2), 3, 3))
  colnames(temp_index) <- 1:3
  s9 <- data.frame(Species =
                     c("A", "D", "B", "B", "E", "C", "B", "A"),
                   Y = c(1, 1, 2, 2, 2, 3, 3, 3),
                   X = c(2, 3, 1, 2, 2, 1, 1, 3))

  realised <- .add_index(s9, 3, temp_index)
  realised <- realised$index

  # Expected values taken from the vignettes S9
  expect_equal(realised, c(2, 3, 4, 5, 5, 7, 7, 9), tolerance = 0.001)

})
