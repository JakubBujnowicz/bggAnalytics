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
.compress <- function(vec, n_show = 5, collapse = ", ")
{
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


#' Add '-s' to the end of a string based on a count
#'
#' Does exactly what the title says.
#'
#' @param string a single string.
#' @param count a single number that determines the addition of "-s".
#'
#' @return A single string.
#' @keywords internal
#'
.plural <- function(string, count)
{
    # Assertions
    assert_that(.is_string(string))
    assert_that(.is_integer(count))

    if (count > 1) {
        string <- paste0(string, "s")
    }

    return (string)
}

