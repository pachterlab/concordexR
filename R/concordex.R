############################
# S4 method definitions
############################

#' Identify Spatially Homogenous Regions with concordex
#'
#' @description Compute the raw and corrected concordex coefficient using a
#'   neighborhood graph and observation labels.
#'
#' @param x A numeric matrix specifying the neighborhood structure of
#'   observations. Typically an adjacency matrix produced by a k-Nearest
#'   Neighbor algorithm. It can also be a matrix whose rows correspond to each
#'   observation and columns correspond to neighbor indices, i.e. matrix form of
#'   an adjacency list which can be a matrix due to fixed number of neighbors.
#' @param labels A numeric or character vector containing the label or class
#'   corresponding to each observation. For example, a cell type or cluster ID.
#' @param k Number of neighbors to expect for each observation. Defaults to 20.
#' @param n.iter A number specifying the number of permutations for correcting
#'   the coefficient.
#' @param return.map Logical, whether to return the matrix of the number of
#'   cells of each label in the neighborhood of cells of each label.
#' @param BPPARAM A \code{\link{BiocParallelParam}} object specifying whether
#'   and how computing the metric for numerous observations shall be
#'   parallelized.
#' @param ... Arguments passed to methods.
#' @returns A named list with the following components:
#' \describe{
#'   \item{`concordex`}{
#'   The raw concordex coefficient corresponding to the original label assignments.
#'   }
#'
#'   \item{`mean_random_concordex`}{
#'   The average of `n.iter` concordex coefficients. concordex coefficients are computed
#'   after permuting the labels and reassigning them to new observations.
#'   }
#'   \item{`corrected_concordex`}{
#'   Simply the raw concordex coefficient divided by the average of the permuted
#'   coefficients.
#'   }
#'   \item{`simulated`}{
#'   Numeric vector of the concordex coefficients from permuted labels, showing the
#'   null distribution.
#'   }
#'   \item{`map`}{
#'   Numeric matrix of the number of cells of each label in the neighborhood of
#'   cells of each label. Only returned when \code{return.map = TRUE}.
#'   }
#' }
#'
#' @export
#' @rdname calculateConcordex
#' @importFrom methods setMethod setGeneric
#' @examples
#'
#' # Simplest case where input is a nxn matrix
#' # Neighbors can be oriented along the rows or columns
#' nCells <- 10
#' k <- 3
#' set.seed(40)
#' labels <- sample(paste0("l", seq_len(3)), nCells, replace=TRUE)
#'
#' mtx <- sapply(seq_len(nCells), function(x) {
#'     out <- rep(0, nCells)
#'     out[-x] <- sample(c(rep(1, k), rep(0, nCells - k - 1)))
#'     out
#' })
#'
#' res <- calculateConcordex(mtx, labels, k = k)
#'
#' res
#'
#' # Also works if input matrix is nxk or kxn
#' mtx <- sapply(seq_len(nCells), function(x) {
#'   out <- sample((seq_len(nCells))[-x], k)
#'   out
#' })
#'
#' res <- calculateConcordex(mtx, labels, k = k)
#'
#' res
setMethod("calculateConcordex", "ANY",
          function(x, labels,
            ...,
            n_neighbors=30,
            compute_similarity=FALSE,
            BLUSPARAM,
            BNINDEX,
            BNPARAM=KmknnParam(),
            BPPARAM=SerialParam()) {

              check_dots_empty()

              check_required(x)
              check_required(labels)

              check_labels(labels, expected=dim(x)[1])

              labels <- labels_make_friendly(labels, nm=dimnames(x)[[1]])

              req_args <- list(
                  x=x,
                  labels=labels,
                  n_neighbors=n_neighbors,
                  compute_similarity=compute_similarity,
                  BNPARAM=BNPARAM,
                  BPPARAM=BPPARAM)

              other_args <- list()

              if (!is_discrete_labels(labels) & compute_similarity) {
                  req_args['compute_similarity'] <- FALSE
                  warn_no_call("Discrete labels are required to compute the similarity matrix.")
              }

              # More argument checking
              if (!missing(BNINDEX)) {
                  other_args[["BNINDEX"]] <- BNINDEX
                  other_args[['precomputed']] <- TRUE
              }
              if (!missing(BLUSPARAM)) {
                  other_args[["BLUSPARAM"]] <- BLUSPARAM
                  other_args[['cluster_neighborhoods']] <- TRUE
              }

              all_args <- c(req_args, other_args)
              do.call(.calculate_concordex, all_args)
          })

setMethod("calculateConcordex", "SummarizedExperiment",
          function(x, labels, ..., assay.type="logcounts") {

              labels <- labels_walk(x, labels)
              calculateConcordex(t(assay(x, i=assay.type)), labels, ...)
          })

setMethod("calculateConcordex", "SingleCellExperiment",
          function(x, labels, ..., use.dimred=NULL) {

              if (!is.null(use.dimred)) {
                  labels <- labels_walk(x, labels, allow.dimred=FALSE)
                  calculateConcordex(reducedDim(x, use.dimred), labels, ...)

              } else {
                  callNextMethod(x=x, labels=labels, ...)
              }
          })

#' @importFrom SpatialExperiment spatialCoords
setMethod("calculateConcordex", "SpatialExperiment",
          function(x, labels, ..., use.spatial=TRUE) {

              labels <- labels_walk(x, labels)

              if (use.spatial) {
                  calculateConcordex(spatialCoords(x), labels=labels, ...)
              } else {
                  callNextMethod(x=x, labels=labels, ...)
              }
          })
