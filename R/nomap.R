############################
# Internal functions
############################
#' @importFrom Matrix rowSums t
.nomap_map <- function(graph){

  groups <- as.factor(rownames(graph))

  out <- rowsum(graph, groups)
  out <- rowsum(t(out), groups)

  # transpose so that rowSums() = K
  t(out)/rowSums(out)
}

.nomap_trace <- function(graph, labels, return.map=FALSE){

  graph <- .set_label_assignments(graph, labels)
  mapped <- .nomap_map(graph)

  mean(diag(mapped))

}

#' @importFrom BiocParallel SerialParam bplapply
.calculate_nomap <- function(
    x, labels, n.iter=15, return.map=FALSE, BPPARAM=SerialParam(), ...){

  .check_graph(x)
  .check_labels(labels)

  trace <- .nomap_trace(x, labels)

  # Permute and correct trace
  trace_random <- bplapply(seq(n.iter), \(ind){
    .nomap_trace(x, sample(labels))
  }, BPPARAM=BPPARAM)

  trace_random <- mean(unlist(trace_random))

  if (return.map) {
    # Not yet implemented
    return(
      list(
        nomap = trace,
        mean_random_nomap = trace_random,
        corrected_nomap = trace/trace_random
      ))
    }

  list(
    nomap = trace,
    mean_random_nomap = trace_random,
    corrected_trace = trace/trace_random
  )
}

############################
# S4 method definitions
############################

#' Compute the Nomap coefficient
#'
#' @description
#' Computes the raw and corrected Nomap coefficient using a neighborhood graph
#' and observation labels.
#'
#' @param x A numeric matrix specifying the neighborhood structure of observations.
#' Typically an adjacency matrix produced by a nearest-neighbor algorithm.
#' @param labels A numeric or character vector containing the label or class
#' corresponding to each observation. For example, a cell type or cluster ID.
#' @param n.iter A number specifying the number of permutations for correcting
#' the coefficient.
#' @param BPPARAM A \code{\link{BiocParallelParam}} object specifying whether
#'   and how computing the metric for numerous observations shall be parallelized.
#' @param ... Not currently used.
#'
#' @returns A named list with the following components:
#' \describe{
#'   \item{`nomap`}{
#'   The raw coefficient corresponding to the original label assignments.
#'   }
#'
#'   \item{`mean_random_nomap`}{
#'   The average of `n.iter` nomap coefficients. Nomap coefficients are computed
#'   after after permuting the labels and reassigning them to new observations.
#'   }
#'   \item{`corrected_nomap`}{
#'   Simply the raw nomap coefficient divided by the average of the permuted
#'   coefficients.
#'   }
#' }
#'
#' @export
#' @rdname calculateNomap

setMethod("calculateNomap", "ANY", .calculate_nomap)


