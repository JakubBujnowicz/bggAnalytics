#' bggAnalytics options
#'
#' The following options are set by default by the package:
#' \itemize{
#'    \item{`bggAnalytics.print`} \[`10`\] --- a single positive integer,
#'      decides how many objects are printed.
#'    \item{`bggAnalytics.verbose`} \[`TRUE`\] --- a logical value,
#'      decides whether additional diagnostic information is printed while
#'      fetching data.
#'    \item{`bggAnalytics.username`} \[`NULL`\] --- a single string,
#'      decides the default username value for [bggCollection] objects.
#' }
#'
#' @name options
#'
NULL


.set_default_options <- function(...)
{
    dots <- list(...)
    dots_names <- names(dots)
    names(dots_names) <- dots_names

    curr_values <- lapply(dots_names, getOption)
    not_set <- names(Filter(is.null, curr_values))
    if (length(not_set) > 0) {
        do.call(options, args = dots[not_set])
    }
}

