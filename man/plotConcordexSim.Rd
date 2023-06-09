% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plotConcordexSim}
\alias{plotConcordexSim}
\title{Plot density plot of simulated results}
\usage{
plotConcordexSim(concordex, ...)
}
\arguments{
\item{concordex}{Output from \code{\link{calculateConcordex}}.}

\item{...}{Other arguments passed to \code{\link{geom_density}}.}
}
\value{
A ggplot2 object. The density plot shows the simulated concordex
coefficient from permuted labels, while the vertical line shows the actual
concordex coefficient.
}
\description{
The concordex values from permuted labels represent the null distribution of the
statistic. This can be plotted as a density plot and visually compared to the
actual value.
}
\examples{
library(BiocNeighbors)
g <- findKNN(iris[, seq_len(4)], k = 10)
res <- calculateConcordex(g$index, labels = iris$Species, k = 10)
plotConcordexSim(res)

}
