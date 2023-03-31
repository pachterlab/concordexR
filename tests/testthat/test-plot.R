library(BiocNeighbors)
library(vdiffr)

suppressWarnings(g <- findKNN(iris[,seq_len(4)], k = 10))
res <- calculateConcordex(g$index, labels = iris$Species, k = 10, return.map = TRUE)

#test_that("Simulation density plot", {
#  testthat::skip_on_ci() # Only for local use because of the nature of permutation
#  expect_doppelganger("Density plot", plotConcordexSim(res))
#})

test_that("Heatmap", {
  expect_error(heatConcordex(res[-5]),
               "Please rerun calculateConcordex with return.map = TRUE.")
  expect_doppelganger("Heatmap", heatConcordex(res))
})
