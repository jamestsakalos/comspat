.plot_data_check <- function(data = NULL, params = NULL, type = NULL,
                             measure = NULL, su_size = NULL,
                             ymin = NULL, ymax = NULL, xmin = NULL, xmax = NULL,
                             p_col = NULL, p_cex = NULL, cex_axis = NULL) {

  # This is a helper function which checks the data structure for potential
  # problems for plotting

  if (is.null(data) | !class(data) == "list")
    stop("data must be of length one of class list")
  if (class(data) == "list" && length(data) >= 2 && !class(p_col) == "list")
    stop("p_col must be a list of length matching data")
  if (is.null(params))
    stop("paramater data is null")
  if (is.null(type) || is.na(match(type, c("Grid", "Transect"))))
    stop("type must be one of Grid or Transect")
  if (is.list(measure) || is.null(measure) ||
      is.na(match(measure, c("CD", "NRC", "AS", "AS_REL")))
      || length(measure) > 1)
    stop("measure must be of length one corresponding to CD, NRC, AS or AS.REL")
  if (is.null(su_size))
    stop("su_size must be a numeric value indicating the size of the smallest
         sampling unit")
  if (is.null(ymin) || is.null(ymax))
    stop("ymin or ymax must be a numeric values inidicating the y-axis extent")

  output <- list(data, params, type,
                 measure, su_size,
                 ymin, ymax, xmin, xmax,
                 p_col, p_cex, cex_axis)

  return(output)
}

.comspat_plot_ci <- function(data = NULL, params = NULL, type = NULL,
                             measure = NULL, su_size = NULL,
                             ymin = NULL, ymax = NULL, xmin = 0.01, xmax = 10,
                             p_col = "black", p_cex = 0.75, cex_axis = 1,
                             xaxt = TRUE, yaxt = TRUE, ci_type = NULL) {

  # This is a helper function to produce a plot with confidence intervals
  # the function accepts a named list from the statistical outputs.

  if (is.null(names(data))) {
    stop("Data must a named list of statistical outputs")
  }

  # determine las argument
  switch(measure,
         "AS" = {
           as_las <- 0
         },
         "AS_REL" = {
           as_las <- 0
         }, {
           as_las <- 2
         }
  )

  # get the x-values
  temp_2 <- params[["Length.of.plots"]] ^ 2 * su_size

  for (i in names(data)) {

    # Plot the original values for the measure
    if (!which(names(data) == i) == 2) {

      temp_o <- data[[i]][[measure]][1, ]

      plot(temp_o ~ temp_2, xaxt = "none", xlim = c(xmin, xmax),
           ylim = c(ymin, ymax), yaxt = "none",
           type = "o", col = "black", cex.axis = 0.75, xlab = "",
           ylab = "", las = as_las, pch = 19, cex = p_cex, log = "x")

    }

    temp_ul <- data[[i]][[measure]][11, ]
    temp_ll <- data[[i]][[measure]][12, ]

    if (ci_type == 'l'){
      lines(temp_ul ~ temp_2, col = p_col[[which(names(data) == i)]])
      lines(temp_ll ~ temp_2, col = p_col[[which(names(data) == i)]])
    }

    if (ci_type == 'py'){
      polygon(c(temp_ul,rev(temp_ll)) ~ c(temp_2,rev(temp_2)),
              col = adjustcolor(p_col[[which(names(data) == i)]],
                                alpha.f=0.5), border = NA)
    }

  }

  if (isTRUE(xaxt)) {
    axis(1,
         at = round(c(0.01, seq(0.02, 0.09, 0.01), 0.1, 1, 10, 100), 2),
         labels = c("", "", "", "", "", "", "", "", "", "", "", "", ""),
         cex.axis = 0.75)
    axis(1, at = round(c(0.01, 0.1, 1, 10, 100), 2),
         labels = c(0.01, 0.1, 1, 10, 100), cex.axis = 0.75)
  }

  if (isTRUE(yaxt) & measure == "NRC") {
    axis(2,
         at = round(c(seq(0, ymax, 10))),
         labels = round(c(seq(0, ymax, 10))),
         cex.axis = 0.75, las = 1)
  }

  if (isTRUE(yaxt) & measure == "CD") {
    axis(2,
         at = round(c(seq(0, ymax, 1))),
         labels = round(c(seq(0, ymax, 1))),
         cex.axis = 0.75, las = 1)
  }

}

##' Within-Community Spatial Organization Plot
##'
##' Function \code{comspat_plot} makes use of core R graphics systems to
##' display several Information Theory metrics. The \code{comspat_plot} does
##' not calculate the actual Information Theory metrics, but it accepts results
##' from the \code{\link{comspat}} function.
##'
##' \code{comspat_plot} is used to construct the initial plot object. It has
##' the functionality to return single or multiple outputs. When multiple
##' outputs are returned data must be supplied as a list; a single measure for
##' each of the data frames will be added to the same plot. This function makes
##' use of core R graphics systems.
##'
##' @param data list of data frames or statistical output returned from
##' \code{comspat}.
##' @param params Data frame providing the secondary sampling information.
##' @param type Character. Supply either \code{"Grid"} or \code{"Transect"}.
##' @param measure Character. Supply one of \code{"CD", "NRC", "AS"}.
##' @param su_size Numeric. Surface area of the smallest sampling unit (mm sq.).
##' @param ymin Numeric. Y axis lower limit.
##' @param ymax Numeric. Y axis upper limit.
##' @param xmin Numeric. Minimum x axis value (i.e. lower range).
##' @param xmax Numeric. Maximum x axis value (i.e. upper range).##'
##' @param p_col list of colors. Single or a vector matching data length.
##' @param p_cex Numeric.
##' @param cex_axis Numeric.
##' @param xaxt TRUE or FALSE. Controls if x-axis text is displayed.
##' @param yaxt TRUE or FALSE. Controls if y-axis text is displayed.
##' @param stats_output TRUE or FALSE. Controls if confidence intervals display.
##' @param ci_type l or py. Confidence interval as line or polygon.
##' @author James L. Tsakalos
##' @seealso \code{\link{comspat}}, \code{\link{data}}
##' @examples
##'
##' # Load the training data and parameter files
##' data("grid_random")
##' data("param_grid")
##'
##' # Perform comspat calculations
##' temp_rand <- comspat(grid_random, param_grid[1:5, ], 64, "Grid")
##'
##' # Plot comspat results
##' comspat_plot(list(temp_rand), param_grid[1:5, ], "Grid",
##' measure = "NRC", su_size = 0.01, ymin = 0, ymax = 65,
##' p_col = list("red"))
##'
##' # Hint - several measures can be combined using par() commands
##' @export

comspat_plot <- function(data = NULL, params = NULL, type = NULL,
                         measure = NULL, su_size = NULL,
                         ymin = NULL, ymax = NULL, xmin = 0.01, xmax = 100,
                         p_col = "black", p_cex = 0.75, cex_axis = 1,
                         xaxt = TRUE, yaxt = TRUE,
                         stats_output = FALSE, ci_type = NULL) {

  output <- .plot_data_check(data, params, type,
                             measure, su_size,
                             ymin, ymax, xmin, xmax,
                             p_col, p_cex, cex_axis)

  if(stats_output == FALSE & is.null(ci_type)){ci_type = 'l'}

  data <- output[[1]]
  params <- output[[2]]
  type <- output[[3]]
  measure <- output[[4]]
  su_size <- output[[5]]
  ymin <- output[[6]]
  ymax <- output[[7]]
  xmin <- output[[8]]
  xmax <- output[[9]]
  p_col <- output[[10]]
  p_cex <- output[[11]]
  cex_axis <- output[[12]]

  if (isFALSE(stats_output)) {
    for (i in 1:seq(data)) {

      temp_1 <- apply(data[[i]][[measure]], 2, max)
      temp_2 <- params[["Length.of.plots"]] ^ 2 * su_size

      if (measure == "AS" || measure == "AS.REL") {
        as_las <- 0
      } else {
        as_las <- 2
      }

      if (i == 1) {

        plot(temp_1 ~ temp_2, xaxt = "none", xlim = c(xmin, xmax),
             ylim = c(ymin, ymax),
             type = "o", col = p_col[[i]][1], cex.axis = 0.75, xlab = "",
             ylab = "", las = as_las, pch = 19, cex = p_cex, log = "x")
        axis(1,
             at = round(c(0.01, seq(0.02, 0.09, 0.01), 0.1, 1, 10, 100), 2),
             labels = c("", "", "", "", "", "", "", "", "", "", "", "", ""),
             cex.axis = 0.75)
        axis(1, at = round(c(0.01, 0.1, 1, 10, 100), 2),
             labels = c(0.01, 0.1, 1, 10, 100), cex.axis = 0.75)

      }else {
        par(new = TRUE)

        plot(temp_1 ~ temp_2, xaxt = "none", xlim = c(xmin, xmax),
             ylim = c(ymin, ymax),
             type = "o", col = p_col[[i]][1], cex.axis = 0.75,
             xlab = "", ylab = "",
             las = as_las, pch = 19, cex = p_cex, log = "x")
      }
    }

    switch(measure[i],
           "CD" = {
             mtext(text = "Compositional Diversity (bits)", side = 2,
                   line = 2, cex = 0.8, font = 2)
           },
           "NRC" = {
             mtext(text = "Realised Combinations (nr.)", side = 2,
                   line = 2, cex = 0.8, font = 2)
           },
           "AS" = {
             mtext(text = "Associatum (bits)", side = 2,
                   line = 2, cex = 0.8, font = 2)
           },
           "AS.REL" = {
             mtext(text = "Rel. Associatum (bits)", side = 2,
                   line = 2, cex = 0.8, font = 2)
           }
    )
  } else {

    .comspat_plot_ci(data, params, type, measure, su_size,
                     ymin, ymax, xmin, xmax, p_col, p_cex, cex_axis,
                     xaxt, yaxt, ci_type = 'l')
  }

}
