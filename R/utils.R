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
    assert_that(is.scalar(collapse), is.character(collapse))

    n <- length(vec)
    extra <- n - n_show
    vec <- vec[seq_len(min(n, n_show))]

    string <- paste0(vec, collapse = collapse)
    if (extra > 0) {
        string <- paste0(string, "... (", extra, " more)")
    }

    return (string)
}
