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
        corrected_trace = trace/trace_random
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

#' @export
#' @rdname calculateNomap
setMethod("calculateNomap", "ANY", .calculate_nomap)


