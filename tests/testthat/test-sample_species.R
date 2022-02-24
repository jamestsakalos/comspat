################################################################################
# Testing - .sample_species helper function
################################################################################

test_that(".sample_species", {

  data("tran_grass_t")
  data <- tran_grass_t
  data <- data[, which(!is.na(match(colnames(data), c("Species", "X"))))]
  data[, "Species"] <- as.factor(as.character(data[, "Species"]))
  data[, "X"] <- as.numeric(as.character(data[, "X"]))
  data[, "value"] <- 1
  data("param_tran")

  temp <- .sample_species(data = data, dim_max = 500,
                  params = param_tran,
                  step = 1, data_r = NULL,
                  type = "Transect", nsp = 6)

  expected <- reshape(data = data, idvar = "Species", timevar = "X",
                    direction = "wide", drop = FALSE)

  expected <- expected[order(expected$Species), ]
  row.names(expected) <- expected$Species
  expected <- expected[, -1]
  expected[is.na(expected)] <- 0
  expected <- as.matrix(expected)
  dimnames(expected) <- NULL
  dimnames(temp) <- NULL

  expect_equal(temp[, -c(1:500)[!c(1:500 %in% unique(data$X))]],
               expected)

})
