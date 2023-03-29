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

#' @importFrom cli cli_abort
.check_graph <- function(graph){
  dims <- dim(graph)

  if (length(unique(dims)) != 1L) {
    cli::cli_abort(
      c("The dimensions of the graph are not equal",
        "i" = "There {?is/are} {dims[1]} row{?s}",
        "i" = "There {?is/are} {dims[2]} column{?s}"),
      call = rlang::caller_env()
    )
  }

  invisible(TRUE)
}
