library(BiocNeighbors)
library(vdiffr)

g <- findKNN(iris[,seq_len(4)], k = 10)
suppressWarnings(res <- calculateNomap(g$index, labels = iris$Species, k = 10, return.map = TRUE))

test_that("Simulation density plot", {
  testthat::skip_on_ci() # Only for local use because of the nature of permutation
  expect_doppelganger("Density plot", plotNomapSim(res))
})

test_that("Heatmap", {
  expect_error(heatNomap(res[-5]),
               "Please rerun calculateNomap with return.map = TRUE.")
  expect_doppelganger("Heatmap", heatNomap(res))
})
