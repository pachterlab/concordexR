############################
# Internal functions
############################

.nomap_trace <- function(graph, labels, return.map=FALSE){
  mapped <- .nomap_map(graph)

  if (return.map){
    # not yet implemented,
    return(mean(diag(mapped)))
  }
  mean(diag(mapped))

}


#' @importFrom Matrix rowSums t
.nomap_map <- function(graph){

  groups <- as.factor(rownames(graph))

  out <- rowsum(graph, groups)
  out <- rowsum(t(out), groups)

  # transpose so that rowSums() = K
  t(out)/rowSums(out)
}

#' @importFrom BiocParallel SerialParam bplapply
.calculate_nomap <- function(graph, labels, n.iter=15, return.map=FALSE, BPPARAM=SerialParam()){

  .check_labels(labels)

  graph <- .set_label_assignments(graph, labels)
  trace <- .nomap_trace(graph, labels)

  # Now shuffle labels and correct the trace
  trace_random <- bplapply(seq(n.iter), \(x){
    .nomap_trace(graph, sample(labels))
  }, BPPARAM=BPPARAM)
  print(trace_random)
  trace_random <- mean(unlist(trace_random))

  if (return.map) {
    # not yet implemented, but should return mapped matrix
    # and nomap statistics
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

#' @export
#' #' @rdname calculateNomap
#' @importFrom SingleCellExperiment reducedDim
setMethod("calculateNomap", "SingleCellExperiment", function(x, labels,..., graph.name=NULL){
  if (is.null(graph.name)) {
    graph.name <- "KNN"
  }

  .calculate_nomap(reducedDim(x, graph.name, withDimnames=TRUE), labels=labels, ...)
})
