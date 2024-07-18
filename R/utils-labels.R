labels_pull <- function(x, labels, ...) {

    if(is.null(x) & !missing(labels)) {
        return(labels)
    }

    if (!is.null(x)) {
        if (length(labels) == 1L) {
            return (x[[labels]])
        } else {
            return(x[,labels])
        }
    }

    stop("Unable to pull labels")
}

#' @importFrom rlang check_required check_dots_empty
labels_walk <- function(x, labels, ..., allow.dimred=TRUE) {

    rlang::check_dots_empty()

    n <- length(labels)

    if (n == 0L) stop("No labels to search for. Must provide labels.")

    if (n == dim(x)[2]) {
        return(labels_pull(NULL, labels=labels))
    }

    if (n == 1L) {
        if (labels %in% reducedDimNames(x) & allow.dimred) {
            type <- labels
            labels <- dimnames(reducedDim(x, type=type))[[2]]

            return((labels_pull(reducedDim(x, type=type), labels=labels)))
        }

        if (labels %in% dimnames(colData(x))[[2]]) {
            return(labels_pull(colData(x), labels=labels))
        }

    } else {

        if (any(labels %in% dimnames(colData(x))[[2]])) {

            which_labels <- intersect(labels, dimnames(colData(x))[[2]])
            return(labels_pull(colData(x), labels=which_labels))
        }
    }
    stop_no_call("The labels were not found in {.arg x}")
}

#' Collapse multiple discrete vectors and one-hot-encode
labels_make_friendly <- function(labels, nm=NULL, sep="_", ...) {

    .type = labels_guess_type(labels)

    if (.type == "type_mixed_compatible_multi" || .type == "type_discrete_multi") {
        labels <- do.call(paste, args=c(labels))
        labels <- gsub(" ", sep, labels)
        .type <- "type_discrete_vector"
    }

    if (.type == "type_discrete_vector") {
        labels <- as.character(labels)
        labels <- Matrix::sparse.model.matrix(~0+labels,sep=sep)

        dimnames(labels)[[2]] <- gsub("labels_", "", dimnames(labels)[[2]])
    }

    labels_set_names(labels, nm=nm)
}

#' Friendly types for continuous/discrete label combinations
labels_guess_type <- function(labels) {

    types <- unique(sapply(labels, typeof))

    if (is_Matrix(labels)) {
        if(is_integer(labels@x)) {
            types <- "integer"
        }
    }

    # Catch rogue intger vectors
    if (is_integer(labels)) {
        types <- "integer"
    }

    out_type <- NULL
    suffix <- if (is.vector(labels)) "vector" else "multi"

    if (length(types) > 1L) {

        if (all(types %in% c("character","integer"))) {
            out_type <- "type_mixed_compatible"

        } else {
            out_type <- "type_mixed_incompatible"
        }
    } else {
        out_type <- switch(types,
            integer   = "type_discrete",
            double    = "type_continuous",
            character = "type_discrete",
            logical   = "type_discrete",
            complex   = "type_continuous",
            NULL
        )
    }

    if (is.null(out_type))
        stop_no_call_internal("Label class has not been determined")

    out_type <- paste(out_type, suffix, sep="_")

    attr(out_type, "labelclass") <-
        if (grepl("discrete", out_type)) "discrete" else "continuous"

    out_type
}


check_labels_type_compatible <- function(labels) {

    .type <- labels_guess_type(labels)

    type_compatible <- function(type) {
        if (grepl("incompatible", .type)) {
            return(FALSE)
        }
        TRUE
    }

    if (!type_compatible(.type)) {
        stop_no_call(
          message = "Label types are not compatible",
          info = "Labels should be discrete {.emph or} continous, not both."
        )
    }
}

check_labels <- function(labels, expected=NULL) {
    dims <- dim(labels) %||% length(labels)

    unique_labels <- unique(labels)
    n_uniq <- dim(unique_labels)[1] %||% length(unique_labels)

    if (n_uniq < 2L) {
        stop_no_call("There are fewer than two unique labels")
    }

    check_labels_type_compatible(labels)

}

is_discrete_labels <- function(labels) {
    lc <- attr(labels_guess_type(labels), "labelclass")

    if (lc == "discrete") {
        return(TRUE)
    }
    if (is.null(ls)) {
        stop_no_call_internal("Label class has not been determined")
    }
    FALSE
}
