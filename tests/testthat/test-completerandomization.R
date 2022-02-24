################################################################################
# Testing - .completerandomization helper function
################################################################################

test_that(".completerandomization works", {

  temp <- data.frame(c(1:3), c(1:3), c(1:3))
  temp <- .completerandomization(temp)

  # difficult to test randomness (but the marginals should be the same)
  expect_equal(sum(temp), 18)

})
