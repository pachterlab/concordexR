library(BiocNeighbors)
library(vdiffr)

suppressWarnings(g <- findKNN(iris[,seq_len(4)], k = 10))
res <- calculateConcordex(g$index, labels = iris$Species, k = 10, return.map = TRUE)

expect_ggplot <- function(g) {
    expect_s3_class(g, "ggplot")
    expect_error(ggplot2::ggplot_build(g), NA)
}

test_that("Simulation density plot", {
    # can't use doppelganger due to the nature of simulation
  expect_ggplot(plotConcordexSim(res))
})

test_that("Heatmap", {
  expect_error(heatConcordex(res[-5]),
               "Please rerun calculateConcordex with return.map = TRUE.")
  expect_doppelganger("Heatmap", heatConcordex(res))
})
