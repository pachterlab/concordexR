############################
# Internal functions
############################
#' @importFrom Matrix rowSums t
#' @importFrom DelayedArray rowsum
.concordex_map <- function(graph) {
    groups <- as.factor(rownames(graph))

    out <- rowsum(graph, groups)
    out <- rowsum(t(out), groups)

    print(out)

    # denominator is the number of observations for each label
    t(out) / colSums(out)
}

.concordex_trace <- function(graph, labels, return.map = FALSE) {
    graph <- .set_label_assignments(graph, labels)
    mapped <- .concordex_map(graph)
    tr <- mean(diag(mapped))
    if (return.map) {
        list(map = mapped, trace = tr)
    } else {
        tr
    }
}

#' @importFrom BiocParallel SerialParam bplapply
#' @importFrom rlang check_required
.calculate_concordex <- function( x, labels, k=20, n.iter=15, return.map = TRUE,
                              BPPARAM=SerialParam()){

    check_required(x)
    check_required(labels)

    .check_labels(labels)
    x <- .check_graph(x, k = k)

    res <- .concordex_trace(x, labels, return.map = TRUE)
    trace <- res$trace

    # Permute and correct trace
    trace_random <- bplapply(seq(n.iter), \(ind){
        .concordex_trace(x, sample(labels), return.map = FALSE)
    }, BPPARAM = BPPARAM)

    sim <- unlist(trace_random)
    trace_random <- mean(sim)


    out <- list(
        concordex = trace,
        mean_random_concordex = trace_random,
        corrected_trace = trace / trace_random,
        simulated = sim
    )
    if (return.map) out <- c(out, map = list(res$map))

    out
}

############################
# S4 method definitions
############################

#' Compute the concordex coefficient
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
#' # Simplest case where input is a nxn matrix
#' # Neighbors can be oriented along the rows or columns
#' nCells <- 10
#' k <- 3
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
          function(x, labels, k=20, n.iter=15, return.map = TRUE,
                   BPPARAM=SerialParam()){
  .check_is_matrix(x)
  .calculate_concordex(x, labels, k, n.iter, return.map, BPPARAM)
})
