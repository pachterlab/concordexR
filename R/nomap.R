############################
# Internal functions
############################
#' @importFrom Matrix rowSums t
#' @importFrom DelayedArray rowsum
.nomap_map <- function(graph) {
    groups <- as.factor(rownames(graph))

    out <- rowsum(graph, groups)
    out <- rowsum(t(out), groups)

    # transpose so that rowSums() = K
    t(out) / rowSums(out)
}

.nomap_trace <- function(graph, labels, return.map = FALSE) {
    graph <- .set_label_assignments(graph, labels)
    mapped <- .nomap_map(graph)
    tr <- mean(diag(mapped))
    if (return.map) {
        list(map = mapped, trace = tr)
    } else {
        tr
    }
}

#' @importFrom BiocParallel SerialParam bplapply
#' @importFrom rlang check_required
.calculate_nomap <- function(
    x, labels, k = 20, n.iter = 15, return.map = FALSE, BPPARAM = SerialParam(), ...) {
    check_required(x)
    check_required(labels)


    .check_labels(labels)
    x <- .check_graph(x, k = k)

    res <- .nomap_trace(x, labels, return.map = TRUE)
    trace <- res$trace

    # Permute and correct trace
    trace_random <- bplapply(seq(n.iter), \(ind){
        .nomap_trace(x, sample(labels), return.map = FALSE)
    }, BPPARAM = BPPARAM)

    sim <- unlist(trace_random)
    trace_random <- mean(sim)


    out <- list(
        nomap = trace,
        mean_random_nomap = trace_random,
        corrected_trace = trace / trace_random,
        simulated = sim
    )
    if (return.map) out <- c(out, map = list(res$map))

    out
}

############################
# S4 method definitions
############################

#' Compute the Nomap coefficient
#'
#' @description Compute the raw and corrected nomap coefficient using a
#'   neighborhood graph and observation labels.
#'
#' @param x A numeric matrix specifying the neighborhood structure of
#'   observations. Typically an adjacency matrix produced by a k-Nearest
#'   Neighbor algorithm.
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
#'
#' @returns A named list with the following components:
#' \describe{
#'   \item{`nomap`}{
#'   The raw nomap coefficient corresponding to the original label assignments.
#'   }
#'
#'   \item{`mean_random_nomap`}{
#'   The average of `n.iter` nomap coefficients. Nomap coefficients are computed
#'   after permuting the labels and reassigning them to new observations.
#'   }
#'   \item{`corrected_nomap`}{
#'   Simply the raw nomap coefficient divided by the average of the permuted
#'   coefficients.
#'   }
#'   \item{`simulated`}{
#'   Numeric vector of the nomap coefficients from permuted labels, showing the
#'   null distribution.
#'   }
#'   \item{`map`}{
#'   Numeric matrix of the number of cells of each label in the neighborhood of
#'   cells of each label. Only returned when \code{return.map = TRUE}.
#'   }
#' }
#'
#' @export
#' @rdname calculateNomap
#' @examples
#' # Simplest case where input is a nxn matrix
#' # Neighbors can be oriented along the rows or columns
#' nCells <- 10
#' k <- 3
#' labels <- sample(paste0("l", seq_len(3)), nCells, replace = TRUE)
#'
#' mtx <- sapply(seq_len(nCells), function(x) {
#'     out <- rep(0, nCells)
#'     out[-x] <- sample(c(rep(1, k), rep(0, nCells - k - 1)))
#'     out
#' })
#'
#' res <- calculateNomap(mtx, labels, k = k)
#'
#' res
#'
#' # Also works if input matrix is nxk or kxn
#' mtx <- sapply(seq_len(nCells), function(x) {
#'     out <- sample((seq_len(nCells))[-x], k)
#'     out
#' })
#'
#' res <- calculateNomap(mtx, labels, k = k)
#'
#' res
setMethod("calculateNomap", "ANY", function(x, ...) {
    .check_is_matrix(x)
    .calculate_nomap(x, ...)
})
