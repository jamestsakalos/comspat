################################################################################
# Testing - .plot_data_check helper function
################################################################################

test_that("plot_data_check error messages match expectation", {

  data("grid_patchy_associated")
  data("param_grid")
  temp <- .comspat_orig(grid_patchy_associated, param_grid[1:3, ],
                        dim_max = 64, type = "Grid")

  # Check NULL data message
  expect_error(.plot_data_check(
    data = NULL, params = NULL, type = NULL,
    measure = NULL, su_size = NULL,
    ymin = NULL, ymax = NULL, xmin = NULL, xmax = NULL,
    p_col = NULL, p_cex = NULL, cex_axis = NULL),
    "data must be of length one of class list", ignore.case = TRUE)

  # Check not list data message
  expect_error(.plot_data_check(
    data = grid_patchy_associated, params = NULL, type = NULL,
    measure = NULL, su_size = NULL,
    ymin = NULL, ymax = NULL, xmin = NULL, xmax = NULL,
    p_col = NULL, p_cex = NULL, cex_axis = NULL),
    "data must be of length one of class list", ignore.case = TRUE)

  # Check list data and p_col have the same length
  expect_error(.plot_data_check(
    data = list(grid_patchy_associated, grid_patchy_associated),
    params = NULL, type = NULL,
    measure = NULL, su_size = NULL,
    ymin = NULL, ymax = NULL, xmin = NULL, xmax = NULL,
    p_col = NULL, p_cex = NULL, cex_axis = NULL),
    "p_col must be a list of length matching data", ignore.case = TRUE)

  # Check NULL parameter message
  expect_error(.plot_data_check(
    data = list(grid_patchy_associated), params = NULL, type = NULL,
    measure = NULL, su_size = NULL,
    ymin = NULL, ymax = NULL, xmin = NULL, xmax = NULL,
    p_col = NULL, p_cex = NULL, cex_axis = NULL),
    "paramater data is null", ignore.case = TRUE)

  # Check NULL type message
  expect_error(.plot_data_check(
    data = list(grid_patchy_associated), params = param_grid[1:3, ],
    type = NULL, measure = NULL, su_size = NULL,
    ymin = NULL, ymax = NULL, xmin = NULL, xmax = NULL,
    p_col = NULL, p_cex = NULL, cex_axis = NULL),
    "type must be one of Grid or Transect", ignore.case = TRUE)

  # Check NULL measure message
  expect_error(.plot_data_check(
    data = list(grid_patchy_associated), params = param_grid[1:3, ],
    type = "Grid", measure = NULL, su_size = NULL,
    ymin = NULL, ymax = NULL, xmin = NULL, xmax = NULL,
    p_col = NULL, p_cex = NULL, cex_axis = NULL),
    "measure must be of length one corresponding to CD, NRC, AS or AS.REL",
    ignore.case = TRUE)

  # Check NULL SU message
  expect_error(.plot_data_check(
    data = list(grid_patchy_associated), params = param_grid[1:3, ],
    type = "Grid", measure = "CD", su_size = NULL,
    ymin = NULL, ymax = NULL, xmin = NULL, xmax = NULL,
    p_col = NULL, p_cex = NULL, cex_axis = NULL),
    "su_size must be a numeric value indicating the size of the smallest
         sampling unit",
    ignore.case = TRUE)

  # Check NULL ymin or ymax message
  expect_error(.plot_data_check(
    data = list(grid_patchy_associated), params = param_grid[1:3, ],
    type = "Grid", measure = "CD", su_size = 0.01,
    ymin = NULL, ymax = NULL, xmin = NULL, xmax = NULL,
    p_col = NULL, p_cex = NULL, cex_axis = NULL),
    "ymin or ymax must be a numeric values inidicating the y-axis extent",
    ignore.case = TRUE)

  # Check NULL ymax message
  temp <- .plot_data_check(
    data = list(grid_patchy_associated), params = param_grid[1:3, ],
    type = "Grid", measure = "CD", su_size = 0.01,
    ymin = 0, ymax = 65, xmin = NULL, xmax = NULL,
    p_col = NULL, p_cex = NULL, cex_axis = NULL)
  expect_type(temp, "list")

})
