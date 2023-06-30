##' Frequency of species combinations
##'
##' The `freq_comb()` function extracts the combinational frequency of species.
##' `freq_comb()` is minor and features in the vignette to assist users
##' visualize their data.
##'
##' @param data data.frame with the frequency of species combinations.
##' @template output_type_param
##' @template output_dim_max_param
##'
##' @return The function returns an object of class data.frame.
##' @author James L. Tsakalos
##' @seealso [`comspat()`]
##' @examples
##'
##' # Training data (see package vignette S.9)
##' s9 <- data.frame(
##'   Species = c("A", "D", "B", "B", "E", "C", "B", "A"),
##'   Y = c(1, 1, 2, 2, 2, 3, 3, 3),
##'   X = c(2, 3, 1, 2, 2, 1, 1, 3)
##' )
##'
##' # create data.frame counting the species combinations
##' freq_comb(
##'   data = s9,
##'   type = "Grid",
##'   dim_max = 3
##' )
##'
##' @export

freq_comb <- function(data, type, dim_max) {

  output <- NULL

  if (type == "Grid") data[, 2] <- paste(data$X, data$Y)

  for (i in unique(data$X)) {

    temp <- data[data$X == i, ]

    if (type == "Grid" & nrow(temp) > 0) {
      temp <- data.frame(
        "combinations" =
          aggregate(temp$Species[order(temp$Species)] ~ ., temp, toString)[3],
        "Plot" = i)
    }

    if (type == "Transect" & nrow(temp) > 0) {
      temp <- data.frame(
        "combinations" =
          aggregate(temp$Species[order(temp$Species)] ~., temp, toString)[2],
        "Plot" = i)
    }

    output <- rbind(output, temp)

  }

  if (type == "Grid") {
    potential <- expand.grid(c(1:dim_max), c(1:dim_max))
    potential <- length(paste(potential$Var1, potential$Var2))
    realised <- length(unique(paste(data$X, data$Y)))
    diff <- potential - realised
  } else {
    potential <- length(c(1:dim_max))
    realised <- length(unique(data$X))
    diff <- potential - realised
  }

  names(output) <- c("combinations", "plot")

  output <- rbind(data.frame("combinations" = rep("Empty", diff),
                           "plot" = rep(999, diff)), output)

  output <- as.data.frame(table(output[1]))
  output[, 2] <- (output$Freq / sum(output$Freq)) * 100
  colnames(output)[1] <- "combinations"
  return(output)
}
