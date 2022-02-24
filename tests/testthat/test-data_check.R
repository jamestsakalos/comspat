################################################################################
# Testing - .data_check helper function
################################################################################

test_that(".data_check grid error messages match expectation", {

  data("grid_patchy_associated")
  data("param_grid")

  # Check NULL data message
  expect_error(.data_check(data = NULL,
                           params = param_grid,
                           dim_max = 64, type = "Grid"),
               "data matrix is null", ignore.case = TRUE)

  # Check data column names
  expect_error(.data_check(data = grid_patchy_associated[, -1],
                           params = param_grid,
                           dim_max = 64, type = "Grid"),
               "data must have Species, X and Y column names",
               ignore.case = TRUE)

  # Check NULL parameter message
  expect_error(.data_check(data = grid_patchy_associated,
                           params = NULL,
                           dim_max = 64, type = "Grid"),
               "paramater data is null", ignore.case = TRUE)

  # Check NULL parameter message
  expect_error(.data_check(data = grid_patchy_associated,
                           params = param_grid[, -1],
                           dim_max = 64, type = "Grid"),
               "paramater data must have Steps.of.scaling,
          Length.of.plots, Height.of.plots column names", ignore.case = TRUE)

  # Check NULL dimension message
  expect_error(.data_check(data = grid_patchy_associated,
                           params = param_grid,
                           dim_max = NULL, type = "Grid"),
               "dim_max must be the number of plots in one row of grid
          or the total number of plots in a transect", ignore.case = TRUE)

  # Check correct type argument
  expect_error(.data_check(data = grid_patchy_associated,
                           params = param_grid,
                           dim_max = 64, type = "wrong"),
               "type must be one of Grid or Transect", ignore.case = TRUE)

  # Check that the function returns a list
  temp <- .data_check(data = grid_patchy_associated,
                      params = param_grid,
                      dim_max = 64, type = "Grid")
  expect_type(temp, "list")

})

test_that(".data_check transect error messages match expectation", {

  data("param_tran")
  data("tran_grass_t")

  # Check data column names
  expect_error(.data_check(data = tran_grass_t[, -1],
                           params = param_grid,
                           dim_max = 500, type = "Transect"),
               "data must have Species and X column names",
               ignore.case = TRUE)

  # Check NULL parameter message
  expect_error(.data_check(data = tran_grass_t,
                           params = param_tran[, -1],
                           dim_max = 500, type = "Transect"),
               "paramater data must have Steps.of.scaling and Length.of.plots",
               ignore.case = TRUE)

})
