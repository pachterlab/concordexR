#' @importFrom cli cli_abort
.check_labels <- function(labels, expected=NULL){
  uniq <- unique(labels)
  n_uniq <- length(uniq)

  if (n_uniq < 2L){

    cli::cli_abort(
      c("Must have at least 2 distinct labels",
        "i" = "There {?is/are} {no(n_uniq)} class label{?s}"),
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
#' @importFrom rlang is_missing is_empty caller_env
#' @importFrom Matrix diag
.check_graph <- function(graph, k,..., call = rlang::caller_env()){

  orientation <- .check_matrix_dims(graph, k=k, return_dims=FALSE)
  graph <- .reorient_matrix(graph, k=k, how=orientation)

  # Check to see if graph is self-referential, warn for now

    diag_s <- sum(diag(graph))
    if (diag_s != 0L){
      cli::cli_warn(
        c("Some nodes in the graph are self-referential",
          "!" = "There should not be an edge between a node and itself"
          ),
        call = call)
    }

    graph
}

#' @importFrom cli cli_abort
.check_is_matrix <- function(x, ..., call = rlang::caller_env()) {
  if (!inherits(x, c("matrix", "Matrix"))) {
    cli::cli_abort("{.arg x} must be a matrix, not a {.cls {class(x)}}.",..., call = call)
  }
}

#' @importFrom cli cli_abort
#' @importFrom Matrix rowSums colSums
.check_matrix_dims <- function(x, k, return_dims=FALSE,..., call = rlang::caller_env()) {
  .check_is_matrix(x, .internal=TRUE)

  dims <- dim(x)

  if (return_dims){
    return(dims)
  }

  guess_orientation <- function(x, k, dims) {
    if (diff(dims) == 0L){
      if (all((rowSums(x)/k)==1)) {return(1)}
      if (all((colSums(x)/k)==1)) {return(2)}
    } else {
      axis <- which(dims == k)
      if (axis == 1) {return(3)}
      if (axis == 2) {return(4)}
    }
    NULL
  }

  pattern <- guess_orientation(x,k=k,dims=dims)

  if (is.null(pattern)) {
    cli::cli_abort("Cannot determine whether neighbors are oriented on the rows or columns",..., call = call)
  }

  switch(pattern, "none", "transpose", "expand_row", "expand_col")
}

#' @importFrom Matrix spMatrix t
.reorient_matrix <- function(x, k, how){
  dims <- .check_matrix_dims(x, return_dims=TRUE)
  r <- dims[1]
  c <- dims[2]

  switch(how,
      "none" = x,
      "transpose" = t(x),
      "expand_row" = sparseMatrix(i=sort(rep(1:c,k)), j=as.vector(x), x=rep(1,c*k), dims=c(c,c)),
      "expand_col" = sparseMatrix(i=rep(1:r,k), j=as.vector(x), x=rep(1,r*k), dims=c(r,r))
  )
}
