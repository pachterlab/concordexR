#' @importFrom cli cli_abort
.check_labels <- function(labels){
  uniq <- unique(labels)
  n <- length(uniq)

  if (n < 2L){

    cli::cli_abort(
      c("Must have at least 2 class labels",
        "i" = "There {?is/are} {no(n)} class label{?s}"),
      call = rlang::caller_env()
    )
  }

  invisible(TRUE)
}

.set_label_assignments <- function(graph, labels){

  dims <- dim(graph)

  dimnames(graph) <- list(labels, labels)

  graph
}

#' @importFrom cli cli_abort cli_warn
.check_graph <- function(graph){

  dims <- dim(graph)
  diag_sum <- sum(diag(graph))

  if (length(unique(dims)) != 1L) {
    cli::cli_abort(
      c("The dimensions of the graph are not equal: ",
        "i" = "There {?is/are} {dims[1]} row{?s}",
        "i" = "There {?is/are} {dims[2]} column{?s}"),
      call = rlang::caller_env()
    )
  }



  # Check to see if graph is self-referential
  # May create separate helper later, warn for now
  if (diag_s != 0L){
    cli::cli_warn(
      c("Some nodes in the graph are self-referential",
      "!" = "There should not be an edge between a node and itself"
    ))
  }

  invisible(TRUE)
}
