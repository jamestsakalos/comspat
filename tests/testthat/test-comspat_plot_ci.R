################################################################################
# Testing - .comspat_plot_ci helper function
################################################################################

test_that("comspat_plot, grid matches expectation", {

  s9 <- data.frame(Species =
                     c("A", "D", "B", "B", "E", "C", "B", "A"),
                   Y = c(1, 1, 2, 2, 2, 3, 3, 3),
                   X = c(2, 3, 1, 2, 2, 1, 1, 3))
  data("param_grid")

  temp <- comspat(s9, param_grid[c(1:2), ], 3, "Grid",
                  randomization_type = "CSR", iterations = 4,
                  measures = c("NRC", "CD", "AS"))

  expect_error(.comspat_plot_ci(list(s9), param_grid[c(1:2), ], "Grid",
                   measure = "CD", su_size = 0.01,
                   ymin = 0, ymax = 5, xmin = 0.01, xmax = 10,
                   p_col = list("black"), p_cex = 0.75, cex_axis = 1,
                   xaxt = TRUE, yaxt = TRUE),
               "Data must a named list of statistical outputs",
               ignore.case = TRUE)

  p <- .comspat_plot_ci(list("CSR" = temp[[2]]), param_grid[c(1:2), ], "Grid",
                        measure = "CD", su_size = 0.01,
                        ymin = 0, ymax = 5, xmin = 0.01, xmax = .1,
                        p_col = list("black"), p_cex = 0.75, cex_axis = 1,
                        xaxt = TRUE, yaxt = TRUE, ci_type = "l")

  vdiffr::expect_doppelganger("grid test plot CD ci", p)

  p <- .comspat_plot_ci(list("CSR" = temp[[2]]), param_grid[c(1:2), ], "Grid",
                   measure = "AS", su_size = 0.01,
                   ymin = 0, ymax = 5, xmin = 0.01, xmax = .1,
                   p_col = list("black"), p_cex = 0.75, cex_axis = 1,
                   xaxt = TRUE, yaxt = TRUE, ci_type = "l")

  vdiffr::expect_doppelganger("grid test plot AS ci", p)

  p <- .comspat_plot_ci(list("CSR" = temp[[2]]), param_grid[c(1:2), ], "Grid",
                        measure = "AS_REL", su_size = 0.01,
                        ymin = 0, ymax = 5, xmin = 0.01, xmax = .1,
                        p_col = list("black"), p_cex = 0.75, cex_axis = 1,
                        xaxt = TRUE, yaxt = TRUE, ci_type = "l")

  vdiffr::expect_doppelganger("grid test plot AS_REL ci", p)

  p <- .comspat_plot_ci(list("CSR" = temp[[2]]), param_grid[c(1:2), ], "Grid",
                        measure = "AS", su_size = 0.01,
                        ymin = 0, ymax = 5, xmin = 0.01, xmax = .1,
                        p_col = list("black"), p_cex = 0.75, cex_axis = 1,
                        xaxt = TRUE, yaxt = TRUE, ci_type = "l")

  vdiffr::expect_doppelganger("grid test plot CD ci", p)

  p <- .comspat_plot_ci(list("CSR" = temp[[2]]), param_grid[c(1:2), ], "Grid",
                        measure = "NRC", su_size = 0.01,
                        ymin = 0, ymax = 10, xmin = 0.01, xmax = .1,
                        p_col = list("black"), p_cex = 0.75, cex_axis = 1,
                        xaxt = TRUE, yaxt = TRUE, ci_type = "l")

  vdiffr::expect_doppelganger("grid test plot NRC ci", p)

  p <- .comspat_plot_ci(list("CSR" = temp[[2]]), param_grid[c(1:2), ], "Grid",
                        measure = "CD", su_size = 0.01,
                        ymin = 0, ymax = 5, xmin = 0.01, xmax = .1,
                        p_col = list("black"), p_cex = 0.75, cex_axis = 1,
                        xaxt = FALSE, yaxt = TRUE, ci_type = "l")

  vdiffr::expect_doppelganger("grid test plot CD ci xaxt FALSE", p)

  p <- .comspat_plot_ci(list("CSR" = temp[[2]]), param_grid[c(1:2), ], "Grid",
                        measure = "CD", su_size = 0.01,
                        ymin = 0, ymax = 5, xmin = 0.01, xmax = .1,
                        p_col = list("black"), p_cex = 0.75, cex_axis = 1,
                        xaxt = FALSE, yaxt = FALSE, ci_type = "l")

  vdiffr::expect_doppelganger("grid test plot CD ci x_yaxt FALSE", p)

})

test_that("comspat_plot, transect matches expectation", {

  data("tran_grass_t")
  data("param_tran")

  expect_error(comspat(tran_grass_t, param_grid[c(1:2), ], 500, "Transect",
                       randomization_type = "CSR", iterations = 4),
               "Height of plots not a valid option for paramater data",
               ignore.case = TRUE)

  temp <- comspat(tran_grass_t, param_tran[c(1:2), ], 500, "Transect",
                  randomization_type = "CSR", iterations = 4,
                  measures = c("NRC", "CD", "AS"))

  p <- .comspat_plot_ci(list("CSR" = temp[[2]]), param_tran[1:2, ], "Transect",
                        measure = "NRC", su_size = 0.01,
                        ymin = 25, ymax = 40, xmin = 0.01, xmax = .06,
                        p_col = list("black"), p_cex = 0.75, cex_axis = 1,
                        xaxt = TRUE, yaxt = TRUE, ci_type = "l")

  vdiffr::expect_doppelganger("transect test plot NRC ci", p)


  p <- .comspat_plot_ci(list("CSR" = temp[[2]]), param_tran[1:2, ], "Transect",
                        measure = "CD", su_size = 0.01,
                        ymin = 0, ymax = 5, xmin = 0.01, xmax = .06,
                        p_col = list("black"), p_cex = 0.75, cex_axis = 1,
                        xaxt = FALSE, yaxt = FALSE, ci_type = "l")

  vdiffr::expect_doppelganger("transect test plot CD ci", p)

})
