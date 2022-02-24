################################################################################
# Testing - comspat_plot
################################################################################

test_that("comspat_plot, grid matches expectation", {

  data("grid_patchy_associated")
  data("param_grid")

  temp <- .comspat_orig(grid_patchy_associated, param_grid[1:3, ],
                        dim_max = 64, type = "Grid")

  p <- comspat_plot(data = list(temp), params = param_grid[1:3, ],
                    type = "Grid", measure = "CD", su_size = 0.01,
                    ymin = 0, ymax = 5, xmin = NULL, xmax = NULL,
                    p_col = list("black"), p_cex = NULL, cex_axis = NULL,
                    xaxt = NULL, yaxt = NULL, stats_output = FALSE)

  vdiffr::expect_doppelganger("grid test CD plot", p)

  p <- comspat_plot(data = list(temp), params = param_grid[1:3, ],
                    type = "Grid", measure = "AS", su_size = 0.01,
                    ymin = 0, ymax = 5, xmin = NULL, xmax = NULL,
                    p_col = list("black"), p_cex = NULL, cex_axis = NULL,
                    xaxt = NULL, yaxt = NULL, stats_output = FALSE)

  vdiffr::expect_doppelganger("grid test AS plot", p)

  p <- comspat_plot(data = list(temp), params = param_grid[1:3, ],
                    type = "Grid", measure = "AS_REL", su_size = 0.01,
                    ymin = 0, ymax = 5, xmin = NULL, xmax = NULL,
                    p_col = list("black"), p_cex = NULL, cex_axis = NULL,
                    xaxt = NULL, yaxt = NULL, stats_output = FALSE)

  vdiffr::expect_doppelganger("grid test AS_REL plot", p)

})

test_that("comspat_plot, transect matches expectation", {

  data("tran_grass_t")
  data("param_tran")

  temp <- .comspat_orig(data = tran_grass_t, params = param_tran[1:3, ],
                        dim_max = 500, type = "Transect")

  q <- comspat_plot(data = list(temp), params = param_tran[1:3, ],
                    type = "Transect", measure = "NRC", su_size = 0.01,
                    ymin = 0, ymax = 40, xmin = NULL, xmax = NULL,
                    p_col = list("black"), p_cex = NULL, cex_axis = NULL,
                    xaxt = NULL, yaxt = NULL, stats_output = FALSE)

  vdiffr::expect_doppelganger("transect test plot", q)

  q <- comspat_plot(data = list(temp), params = param_tran[1:3, ],
                    type = "Transect", measure = "AS", su_size = 0.01,
                    ymin = 0, ymax = 40, xmin = NULL, xmax = NULL,
                    p_col = list("black"), p_cex = NULL, cex_axis = NULL,
                    xaxt = NULL, yaxt = NULL, stats_output = FALSE)

  vdiffr::expect_doppelganger("transect test AS plot", q)

  q <- comspat_plot(data = list(temp), params = param_tran[1:3, ],
                    type = "Transect", measure = "AS_REL", su_size = 0.01,
                    ymin = 0, ymax = 40, xmin = NULL, xmax = NULL,
                    p_col = list("black"), p_cex = NULL, cex_axis = NULL,
                    xaxt = NULL, yaxt = NULL, stats_output = FALSE)

  vdiffr::expect_doppelganger("transect test AS_REL plot", q)

})

test_that("comspat_plot, transect matches expectation, ci", {

  s9 <- data.frame(Species =
                     c("A", "D", "B", "B", "E", "C", "B", "A"),
                   Y = c(1, 1, 2, 2, 2, 3, 3, 3),
                   X = c(2, 3, 1, 2, 2, 1, 1, 3))
  data("param_grid")

  temp <- comspat(s9, param_grid[c(1:2), ], 3, "Grid",
                  randomization_type = "CSR", iterations = 4)

  q <- comspat_plot(data = list("CSR" = temp[[2]]), params = param_grid[1:2, ],
                    type = "Grid", measure = "NRC", su_size = 0.01,
                    ymin = 0, ymax = 40, xmin = NULL, xmax = NULL,
                    p_col = list("black"), p_cex = NULL, cex_axis = NULL,
                    xaxt = NULL, yaxt = NULL, stats_output = TRUE)

  vdiffr::expect_doppelganger("transect test NRC ci plot", q)

})
