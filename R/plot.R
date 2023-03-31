#' Plot density plot of simulated results
#'
#' The nomap traces from permuted labels represent the null distribution of the
#' trace. This can be plotted as a density plot and visually compared to the
#' actual value.
#'
#' @param nomap Output from \code{\link{calculateNomap}}.
#' @param ... Other arguments passed to \code{\link{geom_density}}.
#' @return A ggplot2 object. The density plot shows the simulated nomap
#'   coefficient from permuted labels, while the vertical line shows the actual
#'   nomap coefficient.
#' @export
#' @importFrom ggplot2 ggplot geom_density geom_vline aes labs
#' @examples
#' library(BiocNeighbors)
#' g <- findKNN(iris[,seq_len(4)], k = 10)
#' res <- calculateNomap(g$index, labels = iris$Species, k = 10)
#' plotNomapSim(res)
#'
plotNomapSim <- function(nomap, ...) {
  df <- data.frame(sim = nomap$simulated)
  ggplot(df, aes(sim)) +
    geom_density(...) +
    geom_vline(xintercept = nomap$nomap) +
    labs(x = "Monte-Carlo simulation of nomap")
}

#' Plot the nomap map matrix as a heatmap
#'
#' The \code{calculateNomap} function returns a matrix showing the number of
#' cells of each label in the neighborhood of cells of each label when argument
#' \code{return.map = TRUE}. This function plots this matrix as a heatmap, which
#' can be used as a clustering diagnostic.
#'
#' @inheritParams plotNomapSim
#' @param ... Other arguments passed to \code{\link{pheatmap}} to customize the
#' plot.
#' @return A \code{pheatmap} object.
#' @export
#' @importFrom pheatmap pheatmap
#' @examples
#' library(BiocNeighbors)
#' g <- findKNN(iris[,seq_len(4)], k = 10)
#' res <- calculateNomap(g$index, labels = iris$Species, k = 10,
#'                       return.map = TRUE)
#' heatNomap(res)
#'
heatNomap <- function(nomap, ...) {
  if (!"map" %in% names(nomap)) {
    stop("Please rerun calculateNomap with return.map = TRUE.")
  }
  pheatmap(nomap$map, color = scales::viridis_pal()(256))
}
