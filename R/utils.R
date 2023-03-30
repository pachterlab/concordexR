#' @importFrom cli cli_abort
.check_labels <- function(labels, expected=NULL){
  uniq <- unique(labels)
  n_uniq <- length(uniq)

  if (n < 2L){

    cli::cli_abort(
      c("Must have at least 2 distinct labels",
        "i" = "There {?is/are} {no(n)} class label{?s}"),
      call = rlang::caller_env()
    )
  }

  # How many labels should there be?
  if (!is.null(expected)){

    n_labels <- length(labels)

    if (n_labels != expected) {
      message <- switch(sign(n_labels-expected)+2,
                        "Too few labels supplied",
                        "You have supplied the appropriate number of labels",
                        "Too many labels supplied"
                        )
      cli::cli_abort(
        c(message,
          "i" = "{expected} label{?s} are required",
          "x" = "You supplied {n_labels} label{?s}"),
        call = rlang::caller_env()
      )}
  }

  invisible(TRUE)
}

.set_label_assignments <- function(graph, labels){

  dims <- dim(graph)
  .check_labels(labels, expected=dims[1])

  dimnames(graph) <- list(labels, labels)

  graph
}

#' @importFrom cli cli_abort cli_warn
#' @importFrom rlang is_missing is_empty
.check_graph <- function(graph){

  if (rlang::is_missing(graph)){
    cli::cli_abort(
      c("Argument {.var x} is missing",
        "*" = "Please supply a graph object"),
      call = rlang::caller_env()
    )
  } else{

    dims <- dim(graph)

    if (length(unique(dims)) != 1L) {
      if (is.null(dims)){
        cli::cli_abort(
          c("Cannot determine the dimensions of the graph",
            "i" = "The class of the object you supplied is {.cls {class(graph)}}",
            "i" = "You must supply a matrix-like object for {.var x}"),
          call = rlang::caller_env()
        )

      } else{
        cli::cli_abort(
          c("The dimensions of the graph are not equal: ",
            "x" = "There {?is/are} {dims[1]} row{?s}",
            "x" = "There {?is/are} {dims[2]} column{?s}"),
          call = rlang::caller_env()
          )
        }
    }

  # Check to see if graph is self-referential
  # May create separate helper later, warn for now
    diag_s <- sum(diag(graph))
    if (diag_s != 0L){
      cli::cli_warn(
        c("Some nodes in the graph are self-referential",
          "!" = "There should not be an edge between a node and itself"
          ),
        call = rlang::caller_env())
    }
    }
  invisible(TRUE)
}
