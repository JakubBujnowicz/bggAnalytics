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
.is_number <- function(x, lb = NULL, ub = NULL)
{
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
.are_numbers <- function(x, lb = NULL, ub = NULL)
{
    result <- is.numeric(x) && length(x) > 0 && !anyNA(x)

    if (result && !is.null(lb)) {
        result <- all(x >= lb)
    }
    if (result && !is.null(ub)) {
        result <- all(x <= ub)
    }

    return (result)
}

#' @rdname assertions
#'
.is_integer <- function(x, lb = NULL, ub = NULL)
{
    .is_number(x, lb = lb, ub = ub) && trunc(x) == x
}

#' @rdname assertions
#'
.are_integers <- function(x, lb = NULL, ub = NULL)
{
    .are_numbers(x, lb = lb, ub = ub) && all(trunc(x) == x)
}

#' @rdname assertions
#'
.is_positive_integer <- function(x, lb = NULL, ub = NULL)
{
    .is_integer(x, lb = lb, ub = ub) && x > 0
}

#' @rdname assertions
#'
.are_positive_integers <- function(x, lb = NULL, ub = NULL)
{
    .are_integers(x, lb = lb, ub = ub) && all(x > 0)
}

#' @rdname assertions
#'
.is_string <- function(x, allowed = NULL)
{
    result <- is.character(x) && length(x) == 1 & !is.na(x)

    if (result && !is.null(allowed)) {
        result <- x %in% allowed
    }
    return (result)
}

#' @rdname assertions
#'
.are_strings <- function(x, allowed = NULL)
{
    result <- is.character(x) && length(x) > 0 & !anyNA(x)

    if (result && !is.null(allowed)) {
        result <- all(x %in% allowed)
    }
    return (result)
}

#' @rdname assertions
#'
.is_boolean <- function(x)
{
    is.logical(x) && length(x) == 1 & !is.na(x)
}

#' @rdname assertions
#'
.are_booleans <- function(x)
{
    is.logical(x) && length(x) > 0 & !anyNA(x)
}

#' @rdname assertions
#'
.is_nodeset <- function(x)
{
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
.normal_message <- function(message)
{
    result <- function(call, env) {
        string <- paste0("'", deparse(call$x), "'", message)
        return(string)
    }

    return (result)
}

#' @rdname assertion_messages
#'
.bounds_message <- function(message)
{
    result <- function(call, env) {
        string <- paste0("'", deparse(call$x), "'", message)

        lb <- eval(call$lb, envir = env)
        ub <- eval(call$ub, envir = env)

        if (!is.null(lb) && !is.null(ub)) {
            string <- paste0(string, " within the [", lb, ", ", ub, "] interval")
        } else {
            if (!is.null(lb)) {
                string <- paste0(string, " larger than or equal to ", lb)
            }
            if (!is.null(ub)) {
                string <- paste0(string, " smaller than or equal to ", ub)
            }
        }

        return(string)
    }

    return (result)
}


# Messages ---------------------------------------------------------------------
on_failure(.is_number) <- .bounds_message(
    " is not a single number (not NA)")

on_failure(.are_numbers) <- .bounds_message(
    " is not a non-empty numeric vector without NAs")

on_failure(.is_integer) <- .bounds_message(
    " is not a single integer (not NA)")

on_failure(.are_integers) <- .bounds_message(
    " is not a non-empty integer vector without NAs")

on_failure(.is_positive_integer) <- .bounds_message(
    " is not a single integer (not NA)")

on_failure(.are_positive_integers) <- .bounds_message(
    " is not a non-empty vector of positive integers without NAs")

on_failure(.is_string) <- function(call, env)
{
    allowed <- eval(call$allowed, envir = env)

    if (is.null(allowed)) {
        string <- paste0("'", deparse(call$x), "'",
                         " is not a single string (not NA)")
    } else {
        string <- paste0("'", deparse(call$x), "'",
                         " is not one of the following strings: (",
                         .to_string(allowed), ")")
    }

    return(string)
}

on_failure(.are_strings) <- function(call, env)
{
    allowed <- eval(call$allowed, envir = env)

    if (is.null(call$allowed)) {
        string <- paste0("'", deparse(call$x), "'",
                         " is not non-empty character vector without NAs")
    } else {
        string <- paste0("'", deparse(call$x), "'",
                         " is not a character vector made of the following strings: (",
                         .to_string(allowed), ")")
    }

    return(string)
}

on_failure(.is_boolean) <- .normal_message(
    " is not a single boolean value (TRUE/FALSE)")

on_failure(.are_booleans) <- .normal_message(
    " is not a non-empty logical vector without NAs")

on_failure(.is_nodeset) <- .normal_message(
    " is not a XML nodeset")
