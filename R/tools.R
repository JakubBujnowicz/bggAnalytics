#' Detect the namestyle of the data table
#'
#' Detects whether the \code{dt} data.table was created by a \code{bggAPI}
#' object with \code{'pretty'} or \code{'classic'} names. Ends with the error if
#' one of them cannot be unequivocally determined. This can be useful when
#' working on a modified table that is no longer directly connected to any
#' \code{bggAPI} object.
#'
#' @param dt a data.table from \code{data} slot of a \code{bggAPI} object.
#'
#' @return A single string.
#'
#' @author Jakub Bujnowicz \email{bujnowiczgithub@@gmail.com}
#' @export
#'
#' @examples
#' gm <- bggGames$new(ids = 167791)
#' detect_namestyle(gm$data)
#'
#' gm$switch_namestyle("pretty")
#' detect_namestyle(gm$data)
#'
#' # Breaks
#' # detect_namestyle(iris)
#'
detect_namestyle <- function(dt)
{
    # Assertions
    assert_that(is.data.frame(dt))

    dt_names <- names(dt)
    cl_names <- var_specs$Variable
    pt_names <- var_specs$PrettyName

    cl_count <- sum(dt_names %in% cl_names)
    pt_count <- sum(dt_names %in% pt_names)

    result <- fcase(cl_count > 0 &
                        pt_count > 0,    "both",
                    cl_count > pt_count, "classic",
                    pt_count > cl_count, "pretty",
                    default =            "zeros")

    dt_name <- deparse(substitute(dt))

    if (result == "zeros") {
        stop("no 'pretty' nor 'classic' names found in '", dt_name, "'")
    } else if (result == "both") {
        stop("found both 'pretty' and 'classic' names in '", dt_name, "'")
    }

    return(result)
}
