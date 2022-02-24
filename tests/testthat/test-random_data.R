################################################################################
# Testing - .random_data helper function
################################################################################

test_that(".random_data matches unit tests for grid", {

  data("grid_patchy_associated")
  data("param_grid")

  # RS randomization
  temp <- .random_data(data = grid_patchy_associated, params = param_grid,
                       dim_max = 64, type = "Grid", randomization_type = "RS",
                       iterations = 2)
  expect_type(temp, "list")

  # CSR randomization
  temp <- .random_data(data = grid_patchy_associated, params = param_grid,
                       dim_max = 64, type = "Grid", randomization_type = "CSR",
                       iterations = 2)
  expect_type(temp, "list")

  # Check correct randomization argument
  expect_error(.random_data(data = grid_patchy_associated,
                            params = param_grid,
                            dim_max = 64,
                            type = "Grid", randomization_type = "wrong",
                            iterations = 2),
               "randomization must be one of CSR or RS", ignore.case = TRUE)

})

test_that(".random_data matches unit tests for transect", {

  data("tran_grass_t")
  data("param_tran")

  # RS randomization
  temp <- .random_data(data = tran_grass_t, params = param_tran, dim_max = 500,
                       type = "Transect", randomization_type = "RS",
                       iterations = 2)
  expect_type(temp, "list")

  # CSR randomization
  temp <- .random_data(data = tran_grass_t, params = param_tran, dim_max = 500,
                       type = "Transect", randomization_type = "CSR",
                       iterations = 2)
  expect_type(temp, "list")

})
