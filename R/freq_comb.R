##' Frequency of species combinations
##'
##' Function \code{freq_comb} extracts the combinational frequency of species.
##' The \code{freq_comb} function is minor and features in the vignette to
##' assist users visualize their data.
##'
##' @param data data.frame with the frequency of species combinations.
##' @param type Character. Supply either \code{"Grid"} or \code{"Transect"}.
##' @param dim_max Numeric. Number of sampling units in one row of a
##' \code{"Grid"} or \code{"Transect"}.
##'
##' @return The function returns an object of class data.frame.
##' @author James L. Tsakalos
##' @seealso \code{\link{comspat}}, \code{\link{data}}
##' @examples
##'
##' # Produce some training data (see package vignette S.9)
##' s9 <- data.frame(Species =
##' c("A", "D", "B", "B", "E", "C", "B", "A"),
##' Y = c(1, 1, 2, 2, 2, 3, 3, 3),
##' X = c(2, 3, 1, 2, 2, 1, 1, 3))
##'
##' # create data.frame counting the species combinations
##' freq_comb(data = s9, type = "Grid", dim_max = 3)
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
