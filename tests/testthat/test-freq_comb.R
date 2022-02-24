################################################################################
# Testing - freq_comb function
################################################################################

test_that("freq_comb matches grid expectation", {

  s9 <- data.frame(Species =
                     c("A", "D", "B", "B", "E", "C", "B", "A"),
                   Y = c(1, 1, 2, 2, 2, 3, 3, 3),
                   X = c(2, 3, 1, 2, 2, 1, 1, 3))

  temp <- freq_comb(data = s9, type = "Grid", dim_max = 3)
  expect_equal(sum(temp$Freq), 100)
  expect_equal(temp[temp[, 1] == "A", 2], 2 / 9 * 100)

})

test_that("freq_comb matches transect expectation", {

  s <- data.frame(Species =
                     c("A", "A", "B"),
                   X = c(1, 2, 2))

  temp <- freq_comb(data = s, type = "Transect", dim_max = 2)
  expect_equal(sum(temp$Freq), 100)
  expect_equal(temp[temp[, 1] == "A", 2], 1 / 2 * 100)

})
