################################################################################
# Testing - .comspat_orig helper function
################################################################################

test_that(".comspat_orig, grids NRC, CD match vignette s9", {

  # This is a test of NRC and CD for a grid with known properties
  # Manual calculation is shown in the vignette

  s9 <- data.frame(Species =
                     c("A", "D", "B", "B", "E", "C", "B", "A"),
                   Y = c(1, 1, 2, 2, 2, 3, 3, 3),
                   X = c(2, 3, 1, 2, 2, 1, 1, 3))
  data("param_grid")

  temp <- .comspat_orig(data = s9, params = param_grid[c(1:2), ], dim_max = 3,
                        type = "Grid")

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

test_that(".comspat_orig, grids NRC, CD, AS, AS_REL match proofs", {

  # This is a test of NRC, CD, AS, AS_REL using simulated data
  # The known results produced using the Fortran and Infothem software
  # Both results were (i.e Fortan and Infothem were identical)

  data("grid_patchy_associated")
  data("param_grid")
  temp <- .comspat_orig(grid_patchy_associated, param_grid[1:10, ],
                        dim_max = 64, type = "Grid",
                        measures = c("NRC", "CD", "AS"))

  # NRC TEST
  expect_type(temp, "list")
  x <- apply(round(temp[["NRC"]], 3), 2, function(x)max(x, na.rm = TRUE))
  y <- c(10, 23, 39, 47, 50, 59, 59, 54, 49, 47)
  names(y) <- c("Step_1", "Step_2", "Step_3", "Step_4", "Step_5",
                "Step_6", "Step_7", "Step_8", "Step_9", "Step_10")
  expect_equal(x, y, tolerance = 0)

  # CD TEST
  x <- apply(round(temp[["CD"]], 3), 2, function(x)max(x, na.rm = TRUE))
  y <- c(1.842, 3.210, 3.813, 4.006, 4.037, 4.046, 4.082, 4.135, 4.156, 4.135)
  names(y) <- c("Step_1", "Step_2", "Step_3", "Step_4", "Step_5",
                "Step_6", "Step_7", "Step_8", "Step_9", "Step_10")
  expect_equal(x, y, tolerance = 0.001)

  # AS TEST
  x <- apply(round(temp[["AS"]], 3), 2, function(x)max(x, na.rm = TRUE))
  y <- c(0.072, 0.324, 0.754, 1.024, 1.201, 1.275, 1.248, 1.149, 1.030, 0.909)
  names(y) <- c("Step_1", "Step_2", "Step_3", "Step_4", "Step_5",
                "Step_6", "Step_7", "Step_8", "Step_9", "Step_10")
  expect_equal(x, y, tolerance = 0.001)

  # AS_rel TEST
  x <- apply(round(temp[["AS_REL"]], 3), 2, function(x)max(x, na.rm = TRUE))
  y <- c(0.039, 0.101, 0.198, 0.256, 0.297, 0.315, 0.306, 0.278, 0.248, 0.220)
  names(y) <- c("Step_1", "Step_2", "Step_3", "Step_4", "Step_5",
                "Step_6", "Step_7", "Step_8", "Step_9", "Step_10")
  expect_equal(x, y, tolerance = 0.001)

})

################################################################################
# Testing - Transect
################################################################################

test_that(".comspat_orig, transects NRC, CD, AS, AS_rel match proofs", {

  # This is a test of NRC, CD, AS, AS_REL using simulated data
  # The known results produced using the Fortran and Infothem software
  # Both results were (i.e Fortan and Infothem were identical)

  data("tran_grass_t")
  data("param_tran")

  temp <- .comspat_orig(data = tran_grass_t, params = param_tran[1:10, ],
                        dim_max = 500, type = "Transect",
                        measures = c("NRC", "CD", "AS"))

  # NRC TEST
  expect_type(temp, "list")
  x <- apply(round(temp[["NRC"]], 3), 2, function(x)max(x, na.rm = TRUE))
  y <- c(29, 35, 37, 37, 33, 30, 26, 27, 26, 24)
  names(y) <- c("Step_1", "Step_2", "Step_3", "Step_4", "Step_5",
                "Step_6", "Step_7", "Step_8", "Step_9", "Step_10")
  expect_equal(x, y, tolerance = 0.001)

  # CD TEST
  x <- apply(round(temp[["CD"]], 3), 2, function(x)max(x, na.rm = TRUE))
  y <- c(3.444, 3.916, 4.125, 4.249, 4.289, 4.327, 4.306, 4.282, 4.125, 3.690)
  names(y) <- c("Step_1", "Step_2", "Step_3", "Step_4", "Step_5",
                "Step_6", "Step_7", "Step_8", "Step_9", "Step_10")
  expect_equal(x, y, tolerance = 0.001)

  # AS TEST
  x <- apply(round(temp[["AS"]], 3), 2, function(x)max(x, na.rm = TRUE))
  y <- c(0.072, 0.077, 0.096, 0.122, 0.131, 0.136, 0.146, 0.151, 0.178, 0.177)
  names(y) <- c("Step_1", "Step_2", "Step_3", "Step_4", "Step_5",
                "Step_6", "Step_7", "Step_8", "Step_9", "Step_10")
  expect_equal(x, y, tolerance = 0.001)

  # AS_rel TEST
  x <- apply(round(temp[["AS_REL"]], 3), 2, function(x)max(x, na.rm = TRUE))
  y <- c(0.021, 0.020, 0.023, 0.029, 0.030, 0.031, 0.034, 0.035, 0.043, 0.048)
  names(y) <- c("Step_1", "Step_2", "Step_3", "Step_4", "Step_5",
                "Step_6", "Step_7", "Step_8", "Step_9", "Step_10")
  expect_equal(x, y, tolerance = 0.001)

})
