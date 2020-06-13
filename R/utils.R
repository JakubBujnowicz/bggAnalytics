#' Compress vector into single string
#'
#' Returns string which shows `n_show` first elements and `vec` and amount
#' of truncated elements.
#'
#' @param vec Atomic vector.
#' @param n_show Positive integer.
#' @param collapse Single string, how to collapse the given vector.
#'
#' @return Single string.
#' @keywords internal
#'
compress <- function(vec, n_show = 5, collapse = ", ") {
    # Assertions
    assert_that(is.atomic(vec))
    assert_that(is.count(n_show))
    assert_that(is.string(collapse))

    n <- length(vec)
    extra <- n - n_show
    vec <- vec[seq_len(min(n, n_show))]

    string <- paste0(vec, collapse = collapse)
    if (extra > 0) {
        string <- paste0(string, "... (", extra, " more)")
    }

    return (string)
}

all_scalars <- function(x) {
    assert_that(is.list(x))

    result <- all(sapply(x, length) == 1)
    return(result)
}

# path_add_* ###################################################################
#' Add params to APIs URL
#'
#' Expand given path by a certain parameter. Values to value should be passed as
#' named variables, function uses internal substituting and deparsing. Correct
#' usage example is \code{test_param <- 1} and then
#' \code{path_add_filter("some_path", test_param)}. The result of that operation
#' is \code{"some_path&test_param=1"}.
#'
#' @param path String, URL to a given API.
#' @param value Variable with a value of parameter.
#'
#' @name path_add
#' @keywords internal
#'
#' @return String \code{path} expanded by \code{value}'s parameter.
#'
NULL

#' @describeIn path_add Value should be logical or NULL. NULL and FALSE change nothing,
#'             TRUE adds \code{&variable=1} at the end.
#'
path_add_flag <- function(path, value) {
    assert_that(is.string(path))
    assert_that(is.logical(value) || is.null(value))

    if (!is.null(value) && value) {
        path <- paste0(path, "&", deparse(substitute(value)), "=1")
    }

    return(path)
}


#' @describeIn path_add Value should be a scalar (positive integer or logical)
#'             or NULL. Logical values get casted to integer, so that path
#'             is expanded by \code{&variable=0} or \code{&variable=1}. Integers
#'             are pasted as is, i.e. for value of 5 we get \code{&variable=5}.
#'
path_add_filter <- function(path, value) {
    # Assertion
    assert_that(is.string(path))

    # Nothing to change
    if (is.null(value)) {
        return(path)
    }

    # So that checks don't break
    assert_that(is.scalar(value))

    param_name <- deparse(substitute(value))

    if (is.logical(value)) {
        path <- paste0(path, "&", param_name, "=", as.numeric(value))
    } else if (is.count(value)) {
        path <- paste0(path, "&", param_name, "=", value)
    } else {
        stop("invalid value argument")
    }

    return(path)
}

# Assertions ###################################################################
#' Assertions
#'
#' Check if \code{x} satisfies given conditions.
#'
#' @param x Object to check.
#' @param limit Single positive integer, sets the upper bound of \code{x}. Does
#' nothing if NULL
#'
#' @name assertions
#'
#' @return Logical vector of length one.
#' @keywords internal
#'
NULL

#' @describeIn assertions Checks if \code{x} is NULL or a single positive integer
#' <= \code{limit} (if limit is not \code{NULL}).
#'
is_nullcount <- function(x, limit = NULL) {
    null_x <- is.null(x)
    result <- null_x || (is.count(x) & noNA(x))
    if (!is.null(limit) & !null_x) {
        assert_that(is.count(limit))

        result <- result && x <= limit
    }

    return(result)
}

on_failure(is_nullcount) <- function(call, env) {
    string <- paste0(deparse(call$x),
                     " is not NULL or a single positive integer")
    if (!is.null(call$limit)) {
        string <- paste0(string, " smaller than or equal to ", deparse(call$limit))
    }

    return(string)
}


#' @describeIn assertions Checks if \code{x} is a single logical, not-NA value or
#' NULL.
#'
is_nullflag <- function(x) {
    result <- is.null(x) || (is.flag(x) & noNA(x))
    return(result)
}

on_failure(is_nullflag) <- function(call, env) {
    string <- paste0(deparse(call$x),
                     " is not NULL or a flag (a length one logical vector)")
    return(string)
}
