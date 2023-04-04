#' Plot density plot of simulated results
#'
#' The concordex values from permuted labels represent the null distribution of the
#' statistic. This can be plotted as a density plot and visually compared to the
#' actual value.
#'
#' @param concordex Output from \code{\link{calculateConcordex}}.
#' @param ... Other arguments passed to \code{\link{geom_density}}.
#' @return A ggplot2 object. The density plot shows the simulated concordex
#'   coefficient from permuted labels, while the vertical line shows the actual
#'   concordex coefficient.
#' @export
#' @importFrom ggplot2 ggplot geom_density geom_vline aes labs
#' @examples
#' library(BiocNeighbors)
#' g <- findKNN(iris[, seq_len(4)], k = 10)
#' res <- calculateConcordex(g$index, labels = iris$Species, k = 10)
#' plotConcordexSim(res)
#'
plotConcordexSim <- function(concordex, ...) {
  df <- data.frame(sim = concordex$simulated)
  sim <- NULL
  ggplot(df, aes(sim)) + geom_density(...) +
    geom_vline(xintercept = concordex$concordex) +
    labs(x = "Monte-Carlo simulation of concordex")
}

#' Plot the concordex map matrix as a heatmap
#'
#' The \code{calculateConcordex} function returns a matrix showing the number of
#' cells of each label in the neighborhood of cells of each label when argument
#' \code{return.map = TRUE}. This function plots this matrix as a heatmap, which
#' can be used as a clustering diagnostic.
#'
#' @inheritParams plotConcordexSim
#' @param ... Other arguments passed to \code{\link{pheatmap}} to customize the
#' plot.
#' @return A \code{pheatmap} object.
#' @export
#' @importFrom pheatmap pheatmap
#' @examples
#' library(BiocNeighbors)
#' g <- findKNN(iris[, seq_len(4)], k = 10)
#' res <- calculateConcordex(g$index,
#'     labels = iris$Species, k = 10,
#'     return.map = TRUE
#' )
#' heatConcordex(res)
#'
heatConcordex <- function(concordex, ...) {
  if (!"map" %in% names(concordex)) {
    stop("Please rerun calculateConcordex with return.map = TRUE.")
  }
  pheatmap(concordex$map, color = scales::viridis_pal()(256), ...)
}
