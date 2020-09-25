.to_string <- function(x) paste0("'", x, "'", collapse = ", ")

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
.compress <- function(vec, n_show = 5, collapse = ", ") {
    # Assertions
    assert_that(is.atomic(vec))
    assert_that(.is_positive_integer(n_show))
    assert_that(.is_string(collapse))

    n <- length(vec)
    extra <- n - n_show
    vec <- vec[seq_len(min(n, n_show))]

    string <- paste0(vec, collapse = collapse)
    if (extra > 0) {
        string <- paste0(string, "... (", extra, " more)")
    }

    return (string)
}

#' Check if all elements of a list are of length 1
#'
#' Checks if lists contains onyl scalar values and can be safely unlisted,
#' maintaining the same length.
#'
#' @param x List.
#'
#' @return Logical flag, TRUE or FALSE.
#' @keywords internal
#'
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
