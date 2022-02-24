################################################################################
# Block 7 Testing - comspat function
################################################################################

test_that("comspat, parallel processing, transect", {

  data("tran_grass_t")
  data("param_tran")

  # Check NULL data message
  expect_error(comspat(tran_grass_t, param_tran[1:3, ], 500, "Transect",
                       measures = "CD", randomization_type = "ogg",
                       iterations = 4),
               "randomization_type must be CSR or RS", ignore.case = TRUE)

  temp <- comspat(tran_grass_t, param_tran[1:3, ], 500, "Transect",
                  measures = "CD", randomization_type = "RS", iterations = 4)

  object <- names(temp)
  expect_equal(object, c("Raw data", "Summary statistics"))
  object <- names(temp$`Summary statistics`)
  expect_equal(object, c("CD"))

  object <- round(temp[["Raw data"]][["CD"]][1, ], 3)
  y <- c(3.444, 3.916, 4.125)
  names(y) <- c("step_1", "step_2", "step_3")
  expect_equal(object, y)

})

test_that("comspat, parallel processing, grid", {

  s9 <- data.frame(Species =
                     c("A", "D", "B", "B", "E", "C", "B", "A"),
                   Y = c(1, 1, 2, 2, 2, 3, 3, 3),
                   X = c(2, 3, 1, 2, 2, 1, 1, 3))
  data("param_grid")

  temp <- comspat(s9, param_grid[c(1:2), ], 3, "Grid",
                  randomization_type = "RS", iterations = 4, alpha = 0.05)

  object <- names(temp)
  expect_equal(object, c("Raw data", "Summary statistics"))
  object <- names(temp$`Summary statistics`)
  expect_equal(object, c("CD", "NRC", "AS", "AS_REL"))

  object <- round(temp[["Raw data"]][["CD"]][1, ], 3)
  y <- c(2.419, 2.725)
  names(y) <- c("step_1", "step_2")
  expect_equal(object, y)

})

test_that("comspat, Grid analysis match vignette s9", {

  s9 <- data.frame(Species =
                     c("A", "D", "B", "B", "E", "C", "B", "A"),
                   Y = c(1, 1, 2, 2, 2, 3, 3, 3),
                   X = c(2, 3, 1, 2, 2, 1, 1, 3))
  data("param_grid")

  temp <- comspat(s9, param_grid[c(1:2), ], 3, "Grid",
                  randomization_type = NULL, iterations = NULL)

  # NRC TEST
  x <- apply(round(temp[["NRC"]], 3), 2, function(x)max(x, na.rm = TRUE))
  y <- c(6, 7)
  names(y) <- c("Step_1", "Step_2")
  expect_equal(x, y, tolerance = 0)

  # CD TEST
  x <- apply(round(temp[["CD"]], 3), 2, function(x)max(x, na.rm = TRUE))
  y <- c(2.419, 2.725)
  names(y) <- c("Step_1", "Step_2")
  expect_equal(x, y, tolerance = 0)

})
