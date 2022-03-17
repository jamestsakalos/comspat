.add_index <- function(data = NULL, dim_max = NULL, data_r = NULL) {

  # This function adds the grid number (wide) to the data (long) as a new column
  # e.g. if species A and B are in grid (1, 1) the new column equals 1 for
  # species A and B which appear as separate rows in the original data.

  # Link the index to the original data
  for (across in 1:dim_max) {
    for (down in 1:dim_max) {
      if (!dim(data[data[, "X"] == across & data[, "Y"] == down, ])[1] == 0) {
        data[data[, "X"] == across & data[, "Y"] == down, "index"] <-
          data_r[down, across]
      }
      data_r[down, across]
    }
  }
  return(data)
}

.data_check <- function(data = NULL, params = NULL,
                        dim_max = NULL, type = NULL) {

  # This is a helper function which checks the data structure for potential
  # problems

  if (is.null(data))
    stop("data matrix is null")
  if (is.null(type) | is.na(match(type, c("Grid", "Transect"))))
    stop("type must be one of Grid or Transect")
  if (is.null(params))
    stop("paramater data is null")
  if (is.null(dim_max))
    stop("dim_max must be the number of plots in one row of grid
          or the total number of plots in a transect")

  if (type == "Grid") {
    if (sum(!is.na(match(colnames(data), c("Species", "X", "Y")))) < 3) {
      stop("data must have Species, X and Y column names")
    }
    data <- data[, which(!is.na(match(colnames(data), c("Species", "X", "Y"))))]
    data[, "Species"] <- as.factor(as.character(data[, "Species"]))
    data[, "X"] <- as.numeric(as.character(data[, "X"]))
    data[, "Y"] <- as.numeric(as.character(data[, "Y"]))

    if (sum(!is.na(match(colnames(params),
                         c("Steps.of.scaling",
                           "Length.of.plots",
                           "Height.of.plots")))) < 3)

      stop("paramater data must have Steps.of.scaling,
          Length.of.plots, Height.of.plots column names")

    params <- params[, which(!is.na(match(colnames(params),
                                          c("Steps.of.scaling",
                                            "Length.of.plots",
                                            "Height.of.plots"))))]

    params <- as.data.frame(params)
    params[, "Steps.of.scaling"] <-
      as.numeric(as.character(params[, "Steps.of.scaling"]))
    params[, "Length.of.plots"] <-
      as.numeric(as.character(params[, "Length.of.plots"]))
    params[, "Height.of.plots"] <-
      as.numeric(as.character(params[, "Height.of.plots"]))

    # Add a column informing how many cells need to be skipped
    params <- cbind(params,
                    (params[, "Length.of.plots"] - rep(1, nrow(params))))
    colnames(params)[4] <- "SkipLN"

  }

  if (type == "Transect") {

    if (sum(!is.na(match(colnames(data), c("Species", "X")))) < 2) {
      stop("data must have Species and X column names")
    }

    data <- data[, which(!is.na(match(colnames(data), c("Species", "X"))))]
    data[, "Species"] <- as.factor(as.character(data[, "Species"]))
    data[, "X"] <- as.numeric(as.character(data[, "X"]))

    if ("Height.of.plots" %in% colnames(params)) {
      stop("Height of plots not a valid option for paramater data")
    }

    if (sum(!is.na(match(colnames(params),
                         c("Steps.of.scaling",
                           "Length.of.plots")))) < 2 & type == "Transect")
      stop("paramater data must have Steps.of.scaling and Length.of.plots")

    params <- params[, which(!is.na(
      match(colnames(params), c("Steps.of.scaling", "Length.of.plots"))))]

    params <- as.matrix(params)
    params[, "Steps.of.scaling"] <-
      as.numeric(as.character(params[, "Steps.of.scaling"]))
    params[, "Length.of.plots"] <-
      as.numeric(as.character(params[, "Length.of.plots"]))

  }

  output <- list(data, params, dim_max)
  return(output)

}

.sample_species <- function(data = NULL, dim_max = NULL, params = NULL,
                            step = NULL, data_r = NULL,
                            type = NULL, nsp = NULL) {

  # This function indicates if the species occurs in the sampling unit
  # The occurrence of the species depends on the secondary sampling step

  if (type == "Grid") {

    temp_2 <- matrix(0, nsp, dim_max ^ 2)
    row.names(temp_2) <- unique(data[, "Species"])

    posit <- 0
    for (across in 1:dim_max) {
      for (down in 1:dim_max) {
        posit <- posit + 1
        if (down <= (dim_max - (params[step, "SkipLN"]))) {
          down_cond <- c(down:(down + params[step, "Height.of.plots"] - 1))
        }else {
          down_cond <- c(down:dim_max, (1:(params[step, "Height.of.plots"] -
                                             length(down:dim_max))))
        }
        if (across <= (dim_max - params[step, "SkipLN"])) {
          across_cond <- c(across:(across +
                                     params[step, "Length.of.plots"] - 1))
        }else {
          across_cond <- c(across:dim_max,
                           (1:(params[step, "Length.of.plots"] -
                                 length(across:dim_max))))
        }

        indice <- matrix(data_r[down_cond, across_cond])
        temp_2[, posit] <- row.names(temp_2) %in%
          data[data[["index"]] %in% as.vector(indice), "Species"]
      }
    }
  } else {

    # Transect sampling

    temp_1 <- matrix(0, nsp, length(0:(params[step, "Length.of.plots"] - 1)))
    row.names(temp_1) <- unique(data[, "Species"])

    temp_2 <- matrix(0, nsp, dim_max)
    row.names(temp_2) <- unique(data[, "Species"])

    for (i in 1:dim_max) {

      if (max(c(i:((i + ncol(temp_1)) - 1))) > dim_max) {

        check <- data[data[, "X"] %in% c(
          i:dim_max, 1:(max(c(i:((i + ncol(temp_1)) - 1))) - dim_max)), ]

        if (dim(check)[1] == 0) {
          temp_2 <- temp_2
        }else {
          data[data[, "X"] %in% c(
            i:dim_max, 1:(max(c(i:((i + ncol(temp_1)) - 1))) - dim_max)), ]

          temp_3 <- aggregate(data[data[, "X"] %in% c(
            i:dim_max, 1:(max(c(i:((i + ncol(temp_1)) - 1))) - dim_max)), ],
            by = list(data[data[, "X"] %in%
                              c(i:dim_max, 1:(max(c(i:((i + ncol(temp_1)) - 1)))
                                              - dim_max)),
                            "Species"]), FUN = length)

          temp_2[row.names(temp_2) %in%
                   data[data[, "X"] %in%
                           c(i:dim_max,
                             1:(max(c(i:((i + ncol(temp_1)) - 1))) -
                                  dim_max)), "Species"], i] <-
            temp_3[, "value"]
        }
      }

      check <- data[data[, "X"] %in% c(i:((i + ncol(temp_1)) - 1)), ]

      if (dim(check)[1] == 0) {
        temp_2 <- temp_2
      }else {
        temp_3 <- aggregate(
          data[data[, "X"] %in%
                  c(i:((i + ncol(temp_1)) - 1)), ], by = list(
                    data[data[, "X"] %in%
                            c(i:((i + ncol(temp_1)) - 1)), "Species"]),
          FUN = length)

        temp_2[row.names(temp_2) %in%
                 data[data[, "X"] %in%
                         c(i:((i + ncol(temp_1)) - 1)), "Species"], i] <-
          temp_3[, "value"]
      }
      temp_2 <- temp_2[order(row.names(temp_2)), ]
    }
  }
  return(temp_2)
}

.jnp <- function(data, dim_max, params, data_r, nsp,
                steps, results, type) {

  if (type == "Transect") data[, "value"] <- 1

  for (step in 1:steps) {

    temp_2 <- .sample_species(data, dim_max, params, step, data_r, type, nsp)

    for (q in 1:dim(temp_2)[1]) {

      # Prepare the data for JNP calculations
      prova2_temp <- temp_2[1:q, ]
      prova2_temp[prova2_temp > 0] <- 1

      if (is.null(dim(prova2_temp)) == TRUE) {
        prova2_temp <- as.data.frame(t(prova2_temp))
        unique_comb <- as.matrix(prova2_temp[, !duplicated(t(prova2_temp))])
      }else {
        unique_comb <- as.matrix(prova2_temp[, !duplicated(t(prova2_temp))])
      }

      # NUMBER OF REALIZED COMBINATIONS
      results[["NRC"]][q, step] <- ncol(unique_comb)

      # LOCAL ENTROPY AND THE LOCAL DISTINCTIVNESS
      le_rand <- rep(0, nrow(prova2_temp[, ]))
      for (i in 1:dim(prova2_temp)[1]) {

        le_rand[i] <- (ncol(prova2_temp[, ]) *
                         log2(ncol(prova2_temp)) -
                         sum(prova2_temp[i, ]) *
                         log2(sum(prova2_temp[i, ])) -
                         (ncol(prova2_temp) -
                            sum(prova2_temp[i, ])) *
                         log2((ncol(prova2_temp) -
                                 sum(prova2_temp[i, ])))) /
          ncol(prova2_temp)
      }

      le_rand[is.na(le_rand)] <- 0
      results[["LD"]][q, step] <- sum(le_rand)

      if (!is.null(unique_comb)) {
        freq <- rep(0, ncol(unique_comb))

        for (z in 1:dim(unique_comb)[2]) {
          a <- unique_comb[, z]
          hascol <- function(prova2_temp, a) {
            colSums(a == prova2_temp) == nrow(prova2_temp)
          }
          freq[z] <- sum(hascol(prova2_temp, a))
        }

        # COMPOSITIONAL DIVERSITY
        p1 <- freq / sum(freq)
        results[["CD"]][q, step] <- sum(abs((p1) * log2((p1))))

        # ASSOCIATUM
        results[["AS"]][q, step] <-
          results[["LD"]][q, step] - results[["CD"]][q, step]

        results[["AS"]][q, step][is.na(results[["AS"]][q, step])] <- 0
        results[["AS"]][q, step][is.nan(results[["AS"]][q, step])] <- 0

        results[["AS_REL"]][q, step] <-
          results[["AS"]][q, step] / results[["CD"]][q, step]

        results[["AS_REL"]][q, step][is.na(results[["AS_REL"]][q, step])] <- 0
        results[["AS_REL"]][q, step][is.nan(results[["AS_REL"]][q, step])] <- 0

      }
    }
  }
  return(results)
}

.reflector <- function(data = data, control = NULL) {

  # This is a helper function used in the NULL models
  # The function reflects the data symmetrically along
  # columns, rows or columns and rows.
  # See the package vignette for description

  if (is.null(control) == TRUE) {
    fliprow <- sample(c("TRUE", "FALSE"), 1)
    flipcol <- sample(c("TRUE", "FALSE"), 1)
    if (fliprow == TRUE) {
      data <- matrix(data[dim(data)[1]:1, ], ncol = dim(data)[2])
    }
    if (flipcol == TRUE) {
      data <- matrix(data[, dim(data)[2]:1], ncol = dim(data)[2])
    }
  }
  if (is.null(control) == FALSE) {
    if (control == "None") {
      data <- data
    }
    if (control == "colz") {
      data <- matrix(data[, dim(data)[2]:1], ncol = dim(data)[2])
    }
    if (control == "rowz") {
      data <- matrix(data[dim(data)[1]:1, ], ncol = dim(data)[2])
    }
    if (control == "colz&rowz") {
      data <- matrix(data[, dim(data)[2]:1], ncol = dim(data)[2])
      data <- matrix(data[dim(data)[1]:1, ], ncol = dim(data)[2])
    }
  }
  return(data)
}

.rotator <- function(data = data, control = NULL) {

  # This is a helper function used in the NULL models
  # The function is used with grids and rotates the data:
  # no rotation, 90 degrees, 180 degrees and 270 degrees.
  # See the package vignette for description.

  nr_rotations <- sample(c(1, 2, 3, 4), 1)
  if (is.null(control) == TRUE) {
    if (nr_rotations == 1) {
      data <- data
    } #No reflection
    if (nr_rotations == 2) {
      data <- t(data)[, dim(data)[2]:1]
    } #90 degrees
    if (nr_rotations == 3) {
      data <- data[dim(data)[1]:1, dim(data)[2]:1]
    } #180 degrees
    if (nr_rotations == 4) {
      data <- t(data)[dim(data)[2]:1, ]
    } #270 degrees
  } else {
    if (control == 1) {
      data <- data
    } #No reflection
    if (control == 2) {
      data <- t(data)[, dim(data)[2]:1]
    } #90 degrees
    if (control == 3) {
      data <- data[dim(data)[1]:1, dim(data)[2]:1]
    } #180 degrees
    if (control == 4) {
      data <- t(data)[dim(data)[2]:1, ]
    } #270 degrees
  }
  return(data)
}

.completerandomization <- function(temp_index = NULL) {

  # This is a helper function used in the NULL models
  # The function performs complete randomization of all species
  # See the package vignette for description.

  out <- temp_index[sample(1:dim(temp_index)[1]), sample(1:dim(temp_index)[2])]
  return(out)
}

.shift <- function(temp_index = NULL, rowz = NULL, colz = NULL) {

  # This is a helper function used in the NULL models
  # The function performs random shift of the species in a grid or transect
  # See the package vignette for description.

  t.new <- temp_index

  if (rowz >= 0) {
    countdown <- rowz
    while (!countdown == 0) {
      t.new <- rbind(t.new[nrow(t.new), ], t.new[1:(nrow(t.new) - 1), ])
      countdown <- countdown - 1
    }
  }
  if (rowz < 0) {
    countdown <- rowz
    while (!countdown == 0) {
      t.new <- rbind(t.new[2:nrow(t.new), ], t.new[1, ])
      countdown <- countdown + 1
    }
  }
  if (colz >= 0) {
    countdown <- colz
    while (!countdown == 0) {
      t.new <- cbind(t.new[, ncol(t.new)], t.new[, 1:(ncol(t.new) - 1)])
      countdown <- countdown - 1
    }
  }
  if (colz < 0) {
    countdown <- colz
    while (!countdown == 0) {
      t.new <- cbind(t.new[, 2:ncol(t.new)], t.new[, 1])
      countdown <- countdown + 1
    }
  }
  return(t.new)
}

.shift_transect <- function(data, dim_max,
                            reflection = "1", shift = "1", control = NULL) {

  # Performs random shift for transect data
  # reflect = 1 defaults to random reflection (y/n)
  # reflect = 2 reflects the data
  # reflect = 3 does not reflect the data
  # shift = 1 defaults to a random shift
  # shift = 2 shifts the species, control specifies where the cut occurs
  # shift = 3 does not shift the data

  data[, "X"] <- as.numeric(as.character(data[, "X"]))

  # This part checks if there are empty sample units
  if (length(unique(data[, "X"])) == dim_max) {
    data <- data[order(data[, "X"]), ]
  } else {
    empty_plots <- data.frame(
      "X" = c(1:dim_max)[!c(1:dim_max) %in% unique(data$X)], "Species" = "")
    data <- rbind(data, empty_plots)
    data <- data[order(data[, "X"]), ]
  }

  # Create column as factor
  data[, "Xn"] <- as.factor(data[, "X"])

  # Create a 'reversed' column
  data[, "Xr"] <- length(
    unique(na.omit(data[, "Xn"]))) + 1 - as.numeric(data[, "Xn"])
  data[, "Xn"] <- as.numeric(data[, "Xn"])
  data[, "Xr"] <- as.numeric(data[, "Xr"])

  # Create a blank data.frame
  temp_index_r <- NULL

  for (sp in unique(data[, "Species"])) {

    # Temporary data to be randomized for each species
    data_r <- data

    # Reflect the data
    switch(reflection,
           "1" = {
             data_r[, "X"] <- data_r[, sample(c("Xn", "Xr"), 1)]
           },
           "2" = {
             data_r[, "X"] <- data_r[, "Xr"]
           },
           "3" = {
             data_r[, "X"] <- data_r[, "Xn"]
           }
    )

    data_r <- data_r[, c("X", "Species")]
    data_r <- data_r[order(data_r$X), ]

    # Shift the position of a species
    switch(shift,
           "1" = {
             y <- sample(seq_len(dim_max), 1)
             a <- data_r[data_r$X %in% seq_len(y), ]
             b <- data_r[!data_r$X %in% a$X, ]
             data_r <- rbind(b, a)
           },
           "2" = {
             a <- data_r[data_r$X %in% seq_len(control), ]
             b <- data_r[!data_r$X %in% a$X, ]
             data_r <- rbind(b, a)
           },
           "3" = {
             data_r <- data_r
           }
           )

    # Create a lookup table to relabel the sample units
    lookup <- data.frame(
      level = levels(as.factor(data_r[, "X"])),
      X = unique(as.factor(data_r[, "X"])))

    # Merge lookup values into the data frame
    data_r <- merge(lookup, data_r, by = "X")
    data_r <- data_r[order(data_r$level), ]
    data_r[, "X"] <- as.numeric(data_r[, "level"])
    data_r$level <- NULL

    # Compile the new locations for each species
    temp_index_r <- rbind(temp_index_r, data_r[data_r[, "Species"] == sp, ])

  }

  temp_index_r <- temp_index_r[order(temp_index_r$X), ]
  row.names(temp_index_r) <- NULL
  temp_index_r <- temp_index_r[!temp_index_r$Species == "", ]
  return(temp_index_r)

}

.randomiser <- function(temp_index = NULL, data = NULL, dim_max = NULL,
                        type = NULL, randomization_type = NULL) {

  if (type == "Grid") {

    if (randomization_type == "RS") {

      data_r <- NULL

      for (sp in unique(data[, "Species"])) {

        ########################################################################
        # Reflect the data

        temp_index_r <- .reflector(data = temp_index)

        ########################################################################
        # Rotate the data

        temp_index_r <- .rotator(temp_index_r)

        ########################################################################
        # Perform the random shift

        move_y <- sample(1:dim_max, 1) - sample(1:dim_max, 1)
        move_x <- sample(1:dim_max, 1) - sample(1:dim_max, 1)

        temp_index_r <- .shift(temp_index = temp_index_r, move_y, move_x)

        ########################################################################
        # link to the original data

        dimnames(temp_index_r) <- list(c(1:dim_max), c(1:dim_max))
        temp_index_r <- as.data.frame(as.table(temp_index_r))
        colnames(temp_index_r) <- c("x", "y", "index")
        temp_index_r[, "index"] <-
          as.numeric(as.character(temp_index_r[, "index"]))

        temp_index_r <- merge(data, temp_index_r, by = "index")
        temp_index_r <- temp_index_r[, c(2, 5, 6, 1)]
        colnames(temp_index_r) <- colnames(data)
        temp_index_r <- temp_index_r[, c(1, 2, 3)]
        temp_index_r[, 2] <- as.numeric(as.character(temp_index_r$X))
        temp_index_r[, 3] <- as.numeric(as.character(temp_index_r$Y))

        data_r <- rbind(data_r, temp_index_r[temp_index_r[, "Species"] == sp, ])

      }

      temp_index_r <- data_r

    } else {
      # Perform complete spatial randomization of the grid data
      temp_index_r <- data
      temp_index_r[, "X"] <- sample(temp_index_r[, "X"])
      temp_index_r[, "Y"] <- sample(temp_index_r[, "Y"])
    }
  } else {

    # Perform transect data randomizations

    if (randomization_type == "RS") {

      # Performs random shift (incl. reflections) for each species

      temp_index_r <- .shift_transect(data, dim_max)

    } else {

      # Perform complete spatial randomization

      data_r <- data[, c("X", "Species")]

      for (sp in unique(data_r[, "Species"])) {
        data_r[data_r$Species == sp, "X"] <-
          sample(c(1:dim_max), size = length(data_r[data_r$Species == sp, "X"]))
      }

      data_r <- data_r[order(data_r$X), ]
      row.names(data_r) <- NULL
      temp_index_r <- data_r

    }
  }
  return(temp_index_r)
}

.random_data <- function(data = NULL, params = NULL, dim_max = NULL,
                         type = NULL, randomization_type = NULL,
                         iterations = 999) {

  # This function produces random data for grid and transect data

  # Checks the input data and returns the correct formats / warnings
  data <- .data_check(data, params, dim_max, type)
  dim_max <- data[[3]]
  params <- data[[2]]
  data <- data[[1]]

  # Separate checkof the randomization_type argument
  if (is.null(randomization_type) |
      is.na(match(randomization_type, c("RS", "CSR"))))
    stop("randomization must be one of CSR or RS")

  if (type == "Grid") {

    temp_index <- t(matrix(data = 1:(dim_max ^ 2), dim_max, dim_max))
    colnames(temp_index) <- 1:dim_max
    data <- .add_index(data, dim_max, temp_index)
    data_r <- list()
    data_r[["Original"]] <- data

    # Store randomized data in the list
    if (randomization_type == "RS") {
      # Random shift randomization
      for (randomisations in 1:iterations) {
        temp <- .randomiser(temp_index, data, dim_max, type, randomization_type)
        temp <- .add_index(temp, dim_max, temp_index)
        data_r[[paste(randomisations)]] <- temp
      }
      out <- data_r
    } else {
      # CSR randomization
      for (randomisations in 1:iterations) {
        temp <- .randomiser(temp_index, data, dim_max, type, randomization_type)
        temp <- .add_index(temp, dim_max, temp_index)
        data_r[[paste(randomisations)]] <- temp
      }
      out <- data_r
    }

  } else {

    # Transect randomization
    data_r <- list()
    data_r[["Original"]] <- data

    for (randomisations in 1:iterations) {
      data_r[[paste(randomisations)]] <-
        .randomiser(temp_index = NULL, data,
                    dim_max, type, randomization_type)
    }
    out <- data_r
  }
  return(out)
}

.comspat_orig <- function(data = NULL, params = NULL, dim_max = NULL,
                         type = NULL, measures = NULL) {

  # This is the comspat function without randomizations or parallel computing
  # Checks the input data and returns the correct formats / warnings
  data <- .data_check(data, params, dim_max, type)

  # Set up the parameters needed for the calculation
  dim_max <- data[[3]]
  params <- data[[2]]
  data <- data[[1]]
  nsp <- length(unique(data[, "Species"]))
  steps <- nrow(params)

  if (is.null(measures) == TRUE) measures <- c("CD", "NRC")

  # Create some empty matrices to fill with the new parameters
  cd_rand <- matrix(0, nsp, steps)
  row.names(cd_rand) <- c(as.character(unique(data[, "Species"])))
  colnames(cd_rand) <- c(paste0(rep("Step_"), 1:steps))
  cd_rand <- cd_rand[order(row.names(cd_rand)), ]
  cd_rand <- as.matrix(cd_rand)
  nrc_rand <- matrix(1, nsp, steps)
  dimnames(nrc_rand) <- dimnames(cd_rand)

  # Create a list object to fill with the new parameters
  results <- list("CD" = cd_rand, "NRC" = nrc_rand, "AS" = cd_rand,
                  "AS_REL" = cd_rand, "LD" = cd_rand)
  rm(cd_rand, nrc_rand)
  names(results) <- c("CD", "NRC", "AS", "AS_REL", "LD")

  ##############################################################################
  ##############################################################################

  # Grid Analysis

  if (type == "Grid") {

    # Create a matrix where each cell has a reference number (i.e. index)
    data_r <- t(matrix(data = 1:(dim_max ^ 2), dim_max, dim_max))
    colnames(data_r) <- 1:dim_max

    # Add the reference number to the original data
    data <- .add_index(data, dim_max, data_r)

    # Perform the jnp calculations on the data
    results <- .jnp(data, dim_max, params, data_r, nsp, steps,
                    results, type)

  }

  ##############################################################################
  ##############################################################################
  # Transect Analysis

  if (type == "Transect") {

    results <- .jnp(data, dim_max, params, data_r = NULL, nsp, steps,
                    results, type)

  } # end transect

  ##############################################################################
  ##############################################################################
  # Preparing output data

  if ("AS" %in% measures) {
    measures <- c(measures, "AS_REL")
  }

  results_final <- results[names(results) %in% measures]
  return(results_final)
}


##' Within-Community Spatial Organization
##'
##' The \code{comspat} function calculates Juhász-Nagy Information Theory
##' models.
##'
##' The \code{comspat} function presents four measures from a family of
##' Information Theory models developed by Juhász-Nagy (1967, 1976, 1984a,
##' 1984b). The measures represent co-existence relationships in multispecies
##' communities. For additional information on the measures please see the
##' package vignette.
##'
##' @param data A matrix or data frame capturing the spatial coordinate(s) of
##' species sampled from a grid or transect. Each row captures the
##' coordinate(s) of a \code{Species}. If the \code{data} was sampled as a
##' Transect only the \code{X} coordinate is required.  If the \code{data} was
##' sampled as a Grid both \code{X} and \code{Y} coordinates are required.
##' @param params Data frame providing the secondary sampling information.
##' @param dim_max Numeric. Number of sampling units in one row of a
##' \code{"Grid"} or \code{"Transect"}.
##' @param type Character. Supply either \code{"Grid"} or \code{"Transect"}.
##' @param measures Vector. List the measures returned by \code{comspat()}. The
##' default option returns the compositional diversity (\code{"CD"}), number of
##' realized species combinations (\code{"NRC"}) and associatum (\code{"AS"}).
##' Relative associatum (\code{"as_rel"}) is returned by default when
##' \code{"AS"} is called.
##' @param randomization_type Character. Supply either \code{"CSR"} or
##' \code{"RS"}. Activating randomization initiates parallel computing.
##' @param iterations Numeric. Number of randomizations. The default is 999.
##' @param alpha Numeric. If (\code{NULL}), p value returned. Else 1 or 0.
##'
##' @return The function returns an object of class list returning named data
##' frames specified by the \code{measures} argument. Each data frame contains
##' species as rows and the steps of scaling as columns.
##' @author James L. Tsakalos
##' @seealso \code{\link{comspat_plot}}
##' @references Juhász-Nagy, P. (1967). On some 'characteristic area' of plant
##' community stands. Proc. Colloq. Inf. Theor. 269-282.
##'
##' Juhász-Nagy, P. (1976). Spatial dependence of plant populations. Part 1.
##' Equivalence analysis (an outline for a new model). Acta Bot. Acad Sci.
##' Hung. 22: 61-78.
##'
##' Juhász-Nagy, P. (1984a). Notes on diversity. Part, I. Introduction. Abstr.
##' Bot. 8: 43-55.
##'
##' Juhász-Nagy, P. (1984b). Spatial dependence of plant populations. Part 2. A
##' family of new models. Acta Bot. Acad Sci. Hung. 30: 363-402.
##'
##' Tsakalos, J.L. et al. (2022). comspat: An R package to analyze
##' within-community spatial organization using species combinations. Ecography.
##' doi: 10.1111/ecog.06216.
##'
##' @examples
##'
##' data("grid_random") #input data frame
##' data("param_grid") #input parameter data frame
##' temp <- comspat(data = grid_random, params = param_grid[1:2, ],
##'  dim_max = 64, type = "Grid")
##' @export

comspat <- function(data = NULL, params = NULL, dim_max = NULL, type = NULL,
                    measures = NULL, randomization_type = NULL,
                    iterations = 999, alpha = NULL) {

  if (is.null(measures) == TRUE) measures <- c("CD", "NRC")
  if (!is.null(randomization_type) &
      sum(!is.na(match(randomization_type, c("CSR", "RS")))) < 1) {
    stop("randomization_type must be CSR or RS")
  }

  if (is.null(randomization_type) == TRUE) {
    return(.comspat_orig(data, params, dim_max, type, measures))
  } else {

    # Perform analyses using randomizations and parallell computing power

    # Prepare data for randomizations
    random_dat <- .random_data(data, params, dim_max, type,
                               randomization_type, iterations)

    # Run randomization
    future::plan(future::multisession) # This part opens the parallel processing

    rand <- future.apply::future_lapply(random_dat, FUN = function(x) {
      .comspat_orig(data = data.frame(x), params, dim_max, type, measures)
    })

    future::plan(future::sequential)

    # Clean the output
    steps <- nrow(params)
    cd <- matrix(0, iterations + 1, steps,
                 dimnames = list(
                   names(rand[]), c(paste0(rep("step_"), 1:steps))))

    out <- list("CD" = cd, "NRC" = cd, "AS" = cd, "AS_REL" = cd)
    statz <- out

    for (i in names(rand[])) {
      for (j in names(rand$Original)) {
        out[[j]][i, ] <- apply(
          matrix(unlist(rand[[i]][j]), ncol = steps), 2, max)
      }
    }

    # Calculate statistics
    vars <- c("Original", "Mean", "Max", "Min",
              "Quartile 1", "Quartile 3", "std", "cv", "p o < r", "p o > r",
              "95%UL", "95%LL")

    cd <- matrix(0, length(vars), steps,
                 dimnames = list(c(vars), c(paste0(rep("step_"), 1:steps))))

    statz <- list("CD" = cd, "NRC" = cd, "AS" = cd, "AS_REL" = cd)

    for (i in names(out[])) {

      # Original value
      statz[[i]]["Original", ] <- out[[i]]["Original", ]

      # Mean value
      statz[[i]]["Mean", ] <-
        apply(matrix(out[[i]][2:(iterations + 1), ], ncol = steps), 2, mean)

      # Max value
      statz[[i]]["Max", ] <-
        apply(matrix(out[[i]][2:(iterations + 1), ], ncol = steps), 2, max)

      # Min value
      statz[[i]]["Min", ] <-
        apply(matrix(out[[i]][2:(iterations + 1), ], ncol = steps), 2, min)

      # Quartile 1
      summy <- matrix(summary(matrix(out[[i]][2:(iterations + 1), ],
                                     ncol = steps)), ncol = steps)
      statz[[i]]["Quartile 1", ] <-
        as.numeric(as.character(trimws(
          sapply(strsplit(summy[2, ], ":"), `[`, 2))))

      # Quartile 3
      statz[[i]]["Quartile 3", ] <- as.numeric(as.character(trimws(
        sapply(strsplit(summy[5, ], ":"), `[`, 2))))

      # Standard deviation
      statz[[i]]["std", ] <- apply(matrix(
        out[[i]][2:(iterations + 1), ], ncol = steps), 2, sd)

      # Coefficient of variation
      statz[[i]]["cv", ] <- statz[[i]][7, ] / statz[[i]][2, ]
      statz[[i]]["cv", ][is.nan(statz[[i]]["cv", ])] <- 0.0000

      for (k in 1:steps) {
        # p-value where observed value is < random reference
        statz[[i]]["p o < r", k] <- sum(
          matrix(out[[i]][2:(iterations + 1), ], ncol = steps)[, k]
          < matrix(out[[i]], ncol = steps)[1, k]) / (iterations)

        # p-value where observed value is > random reference
        statz[[i]]["p o > r", k] <- sum(
          matrix(out[[i]][2:(iterations + 1), ], ncol = steps)[, k]
          > matrix(out[[i]], ncol = steps)[1, k]) / (iterations)
      }

      # 95% upper limit confidence interval
      statz[[i]]["95%UL", ] <- statz[[i]][6, ]

      # 95% lower limit confidence interval
      statz[[i]]["95%LL", ] <- statz[[i]][5, ] #LL (lower limit)

      # Assign true or false depending on the alpha value
      if (is.null(alpha) == FALSE) {
        ifelse(statz[[i]]["p o > r", ] <= alpha, 1, 0)
        ifelse(statz[[i]]["p o < r", ] <= alpha, 1, 0)
      }

      statz[[i]] <- round(statz[[i]], 4)

    }

    if ("AS" %in% measures) {
      measures <- c(measures, "AS_REL")
    }

    out <- out[measures]
    statz <- statz[measures]

    return(list("Raw data" = out, "Summary statistics" = statz))

  }

}
