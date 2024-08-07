#' Return default value if 'x' is null
#' @noRd
'%||%' <- function(x, y) {
    if (is.null(x)) x <- y
    x
}

#' alias for `attributes`
#' @noRd
attrs <- attributes

check_is_matrix <- function(x, ..., call = rlang::caller_env()) {
    if (!inherits(x, c("matrix", "Matrix"))) {
        stop_no_call("{.arg x} must be a matrix, not a {.cls {class(x)}}.",
                     .envir=rlang::current_env())
    }
}

#' Are all entries in the input numeric?
#' @noRd
check_all_numeric <- function(x) {
    all_numeric <- vapply(x, is.numeric,logical(1))

    if (!all(all_numeric))
        stop_no_call(
            "All entries in {.arg x} must be numeric.",
            info="Here are the indices of the problematic columns/entries: {which(!all_numeric)}",
            .envir=rlang::current_env())
}

#' @importFrom cli cli_abort cli_warn
#' @noRd
message_handler <- function(suffix, call=NULL, internal=FALSE) {

    cli_fun <- paste0("cli_", suffix)
    function(message,
             info=NULL,
             success=NULL,
             failure=NULL,
             error_bullets=NULL,
             ...) {

        message <- c(
            message,
            "i"=info,
            "v"=success,
            "x"=failure,
            "!"=error_bullets
        )

        dots <- list(...)

        cli_args <- list(
            message=message,
            call=call,
           .internal=internal
        )

        do.call(cli_fun, args = c(cli_args, dots))
    }
}


stop_no_call_internal <- message_handler("abort", internal=TRUE)
warn_no_call_internal <- message_handler("warn", internal=TRUE)

stop_no_call <- message_handler("abort")
warn_no_call <- message_handler("warn")

nullify_if <- function(predicate_fun, ...) {
    dots <- list(...)

    function(x, y) {
        cnd <- do.call(predicate_fun, c(list(x=x), dots))
        if (cnd) {
            return(NULL)
        }
        y
    }
}

#' If object `x` is a vector, return `y`
#' @noRd
nullify_if_vector <- nullify_if(is.vector)
nullify_if_data_frame <- nullify_if(is_frame_object)


inherits_from <- function(options) {
    function(x) inherits(x, options)
}

is_frame_object <- inherits_from(c("data.frame", "DFrame", "DataFrame"))
is_Matrix <- inherits_from("Matrix")
is_numeric <- inherits_from(c("Matrix","numeric"))


is_integer <- function(x, tol=.Machine$double.eps^0.5) {
    if (is_numeric(x)) {
        return(all(abs(x - round(x)) < tol))
    }
    FALSE
}



#' Ensure object is named
#' @noRd
named <- function(x, nm, margin=1L) {

    if (is.vector(x)) {
        return(rlang::set_names(x, nm))
    } else {
        dimnames(x)[[margin]] <- nm
        return(x)
    }
}

#' Set names to margin of a matrix or update names of a vector
#' @noRd
set_names_handler <- function(allow_missing_nm=TRUE, allow_null_nm=TRUE, margin=1L) {

       function(x, nm, ...) {

           dims <- dim(x) %||% length(x)

           if (margin > length(dims))
               stop_no_call_internal("{.arg margin} does not match dimensions of {.arg x}")

           if (!allow_null_nm & allow_missing_nm) allow_missing_name <- FALSE

           if ((allow_missing_nm & missing(nm)) || (allow_null_nm & is.null(nm))) {

               # keep existing margin names
               nm_existing <- dimnames(x)[[margin]] %||% nullify_if_data_frame(x, names(x))
               nm <- nm_existing %||% paste0("...", seq_len(dims[margin]))

           } else if (!allow_missing_nm & missing(nm)) {
               if (allow_null_nm) {
                   stop_no_call_internal("{.arg nm} Must be supplied. ")
               }
               stop_no_call("{.arg nm} Must be supplied or {.var NULL}. ")
           } else if (!allow_null_nm & is.null(nm)) {
               stop_no_call_internal("{.arg nm} Must be supplied and cannot be {.var NULL}. ")
           }

           named(x, nm, margin)
       }
}

labels_set_names <- set_names_handler(allow_missing_nm=FALSE)
row_set_names <- set_names_handler(margin=1)
col_set_names <- set_names_handler(margin=2)
