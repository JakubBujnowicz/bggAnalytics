# Assertion tools --------------------------------------------------------------
#' Assertions
#'
#' Check if \code{x} satisfies given conditions.
#'
#' @param x an object to check.
#' @param lb,ub single numbers - (closed) bounds of \code{x}. Does
#' nothing if \code{NULL}.
#'
#' @name assertions
#'
#' @return Logical vector of length one.
#' @keywords internal
#'
NULL

#' @rdname assertions
#'
.is_number <- function(x, lb = NULL, ub = NULL) {
    result <- is.numeric(x) && length(x) == 1 && !is.na(x)

    if (result && !is.null(lb)) {
        result <- x >= lb
    }
    if (result && !is.null(ub)) {
        result <- x <= ub
    }

    return (result)
}

#' @rdname assertions
#'
.are_numbers <- function(x) {
    is.numeric(x) && length(x) > 0 && !anyNA(x)
}

#' @rdname assertions
#'
.is_integer <- function(x, lb = NULL, ub = NULL) {
    .is_number(x, lb = lb, ub = ub) && trunc(x) == x
}

#' @rdname assertions
#'
.are_integers <- function(x) {
    .are_numbers(x) && all(trunc(x) == x)
}

#' @rdname assertions
#'
.is_positive_integer <- function(x, lb = NULL, ub = NULL) {
    .is_integer(x, lb = lb, ub = ub) && x > 0
}

#' @rdname assertions
#'
.are_positive_integers <- function(x) {
    .are_integers(x) && all(x > 0)
}

#' @rdname assertions
#'
.is_string <- function(x) {
    is.character(x) && length(x) == 1 & !is.na(x)
}

#' @rdname assertions
#'
.are_strings <- function(x) {
    is.character(x) && length(x) > 0 & !anyNA(x)
}

#' @rdname assertions
#'
.is_boolean <- function(x) {
    is.logical(x) && length(x) == 1 & !is.na(x)
}

#' @rdname assertions
#'
.are_booleans <- function(x) {
    is.logical(x) && length(x) > 0 & !anyNA(x)
}

#' @rdname assertions
#'
.is_nodeset <- function(x) {
    inherits(x, what = "xml_nodeset")
}

# Message functions ------------------------------------------------------------
#' Function closures for assertion failure messages
#'
#' Set of function closures made for easy assertion messages.
#'
#' @param message a single string.
#'
#' @name assertion_messages
#'
#' @return A function \code{f} valid for \code{on_failure(fun) <- f} assignment.
#' @keywords internal
#'
NULL

#' @rdname assertion_messages
#'
.normal_message <- function(message) {
    result <- function(call, env) {
        string <- paste0(deparse(call$x),
                         )
        return(string)
    }

    return (result)
}

#' @rdname assertion_messages
#'
.bounds_message <- function(message) {
    result <- function(call, env) {
        string <- paste0(deparse(call$x), message)

        if (!is.null(call$lb) && !is.null(call$ub)) {
            string <- paste0(string, " within the [", deparse(call$lb),
                             ", ", deparse(call$ub), "] interval")
        } else {
            if (!is.null(call$lb)) {
                string <- paste0(string, " larger than or equal to ",
                                 deparse(call$lb))
            }
            if (!is.null(call$ub)) {
                string <- paste0(string, " smaller than or equal to ",
                                 deparse(call$ub))
            }
        }

        return(string)
    }

    return (result)
}


# Messages ---------------------------------------------------------------------
on_failure(.is_number) <- .bounds_message(
    " is not a single number (not NA)")

on_failure(.are_numbers) <- .normal_message(
    " is not a non-empty numeric vector without NAs")

on_failure(.is_integer) <- .bounds_message(
    " is not a single integer (not NA)")

on_failure(.are_integers) <- .normal_message(
    " is not a non-empty integer vector without NAs")

on_failure(.is_positive_integer) <- .bounds_message(
    " is not a single integer (not NA)")

on_failure(.are_positive_integers) <- .normal_message(
    " is not a non-empty vector of positive integers without NAs")

on_failure(.is_string) <- .normal_message(
    " is not a single string (not NA)")

on_failure(.are_strings) <- .normal_message(
    " is not a non-empty character vector without NAs")

on_failure(.is_boolean) <- .normal_message(
    " is not a single boolean value (TRUE/FALSE)")

on_failure(.are_booleans) <- .normal_message(
    " is not a non-empty logical vector without NAs")

on_failure(.is_nodest) <- .normal_message(
    " is not a XML nodeset")
