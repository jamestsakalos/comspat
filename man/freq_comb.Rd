% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/freq_comb.R
\name{freq_comb}
\alias{freq_comb}
\title{Frequency of species combinations}
\usage{
freq_comb(data, type, dim_max)
}
\arguments{
\item{data}{data.frame with the frequency of species combinations.}

\item{type}{Character. Supply either \code{"Grid"} or \code{"Transect"}.}

\item{dim_max}{Numeric. Number of sampling units in one row of a
\code{"Grid"} or \code{"Transect"}.}
}
\value{
The function returns an object of class data.frame.
}
\description{
Function \code{freq_comb} extracts the combinational frequency of species.
The \code{freq_comb} function is minor and features in the vignette to
assist users visualize their data.
}
\examples{

# Produce some training data (see package vignette S.9)
s9 <- data.frame(Species =
c("A", "D", "B", "B", "E", "C", "B", "A"),
Y = c(1, 1, 2, 2, 2, 3, 3, 3),
X = c(2, 3, 1, 2, 2, 1, 1, 3))

# create data.frame counting the species combinations
freq_comb(data = s9, type = "Grid", dim_max = 3)

}
\seealso{
\code{\link{comspat}}, \code{\link{data}}
}
\author{
James L. Tsakalos
}
