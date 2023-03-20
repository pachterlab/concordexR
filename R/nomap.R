#' Title
#'

#'
.calculate_nomap <- function(graph, labels, return.map=FALSE){

  .check_labels(labels)

  graph <- .set_label_assignments(graph, labels)
  mapped <- .nomap_map(graph)

  if (return.map) {
    print('here')
  }

  mean(diag(mapped))

}

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

############################
# S4 method definitions
############################

#' @export
#' @rdname calculateNomap
setMethod("calculateNomap", "ANY", .calculate_nomap)
setMethod("calculateNomap", "SingleCellExperiment", function(x, labels,..., graph.name=NULL){
  if (is.null(graph.name)) {
    graph.name <- "KNN"
  }

  .calculate_nomap(reducedDim(x, graph.name, withDimnames=TRUE), labels=labels, ...)
})
