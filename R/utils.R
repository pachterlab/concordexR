#' Return default value if 'x' is null
'%||%' <- function(x, y) {
    if (is.null(x)) x <- y
    x
}

#' alias for `attributes`
attrs <- attributes

check_is_matrix <- function(x, ..., call = rlang::caller_env()) {
    if (!inherits(x, c("matrix", "Matrix"))) {
        stop_no_call("{.arg x} must be a matrix, not a {.cls {class(x)}}.",
                     .envir=rlang::current_env())
    }
}


#' @importFrom cli cli_abort
stop_handler <- function(call=NULL, internal=FALSE) {
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

        cli_abort(message, call=call, .internal=internal, ...)
    }
}

#' @importFrom cli cli_warn
warn_handler <- function(call=NULL, internal=FALSE) {
    function(message,
             info=NULL, success=NULL,
             failure=NULL,error_bullets=NULL) {

        message <- c(
            message,
            "i"=info,
            "v"=success,
            "x"=failure,
            "!"=error_bullets
        )

        cli_warn(message, call=call, .internal=internal)
    }
}

stop_no_call_internal <- stop_handler(internal=TRUE)
warn_no_call_internal <- warn_handler(internal=TRUE)

stop_no_call <- stop_handler()
warning_no_call <- warn_handler()

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

#' Is the object some flavor of a data frame?
is_frame_object <- function(x) {
    options <- c("data.frame", "DFrame", "DataFrame")

    inherits(x, what=options)
}

#' If object `x` is a vector, return `y`
nullify_if_vector <- nullify_if(is.vector)
nullify_if_data_frame <- nullify_if(is_frame_object)

#' Ensure object is named
named <- function(x, nm, margin=1L) {

    if (is.vector(x)) {
        return(rlang::set_names(x, nm))
    } else {
        dimnames(x)[[margin]] <- nm
        return(x)
    }
}

#' Set names to margin of a matrix or update names of a vector
set_names_handler <- function(allow_missing_nm=TRUE, allow_null_nm=TRUE, margin=1L) {

       function(x, nm, ...) {

           dims <- dim(x) %||% length(x)

           if (margin > length(dims))
               stop_no_call("{.arg margin} does not match dimensions of {.arg x}")

           if ((allow_missing_nm & missing(nm)) || (allow_null_nm & is.null(nm))) {
               # keep existing margin names
               nm_existing <- dimnames(x)[[margin]] %||% nullify_if_data_frame(x, names(x))
               nm <- nm_existing %||% paste0("...", seq_len(dims[margin]))
           } else {
               stop_no_call("{.arg nm} Must be supplied and cannot be {.val NULL}")
           }

           if (dims[margin] != length(nm)) {
               stop_no_call(
                   "{length(nm)} label{?s} were supplied, but {dims[margin]} are required.",
                   .envir=rlang::current_env()
                )
           }

           named(x, nm, margin)
       }
}

row_set_names <- set_names_handler(margin=1)
col_set_names <- set_names_handler(margin=2)
