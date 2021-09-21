#' Get BoardGameGeek URLs for games with given IDs
#'
#' This function is a simple wrapper that returns URLs to given games by using
#' their IDs.
#'
#' @param ids a numeric vector of positive integers.
#'
#' @return A character vector of the same length as \code{x}, contains URLs.
#'
#' @author Jakub Bujnowicz \email{bujnowiczgithub@@gmail.com}
#' @export
#'
#' @examples
#' bgg_gameurl(1:10)
#'
bgg_gameurl <- function(ids)
{
    # Assertions
    assert_integerish(ids, lower = 1, min.len = 1)

    result <- paste0(.bgg_url("boardgame"), ids)
    return(result)
}


#' Merge data from two bggAPI objects
#'
#' This allows for merging data from two bggAPI objects. Namestyle is inherited
#' from \code{x} and columns from \code{y$data} are added to \code{x$data}
#' (unless they are already there).
#'
#' @param x,y objects that inherit from \code{bggAPI} class.
#' @param ... other arguments passed to \code{\link[data.table]{merge}}.
#'
#' @return A data.table with variables from both \code{x} and \code{y}.
#' @export
#'
#' @examples
#'
#' sr <- bggSearch$new("Terraforming Mars")
#' gm <- bggGames$new(sr$ids)
#'
#' gm$expand(c("name", "type", "rank"))
#'
#' bgg_merge(sr, gm)
#'
bgg_merge <- function(x, y, ...)
{
    # Assertions
    assert_class(x, "bggAPI")
    assert_class(y, "bggAPI")

    # Use pretty names?
    pn <- x$params$pretty_names
    pn_style <- fifelse(pn, "pretty", "classic")
    y$switch_namestyle(pn_style)

    # Get data
    xdata <- x$data
    ydata <- y$data

    # Get cols from y that are missing in x
    y_cols <- c(key(ydata), setdiff(names(ydata), names(xdata)))
    ydata <- ydata[, y_cols, with = FALSE]

    result <- merge(xdata, ydata, ...)
    return(result)
}


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
#' bgg_namestyle(gm$data)
#'
#' gm$switch_namestyle("pretty")
#' bgg_namestyle(gm$data)
#'
#' # Breaks
#' # bgg_namestyle(iris)
#'
bgg_namestyle <- function(dt)
{
    # Assertions
    assert_data_frame(dt)

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


#' Get IDs of top rated games on BoardGameGeek
#'
#' This function scraps BoardGameGeek website for IDs of games with given
#' \code{places} in the games ranking.
#'
#' @param places a numeric vector of positive integers.
#'
#' @return Numeric vector of IDs.
#'
#' @author Jakub Bujnowicz \email{bujnowiczgithub@@gmail.com}
#' @export
#'
#' @examples
#' bgg_topgames()
#'
#' x <- 1:10 * 25 + 5
#' ids <- bgg_topgames(sample(x))
#' gm <- bggGames$new(ids)
#' gm$expand(c("name", "rank"))
#' gm
#'
bgg_topgames <- function(places = 1:100)
{
    # Assertions
    assert_integerish(places, lower = 1, min.len = 1)

    pages <- ceiling(places / 100)
    pages <- split(places, pages)

    page_no <- as.numeric(names(pages))

    result <- character()
    for (i in seq_along(pages)) {
        page <- page_no[i]

        xml <- read_html(paste0(.bgg_url("ranking"), "/page/", page))
        xml <- xml_find_all(xml, xpath = "//*[@class = 'primary']")

        hrefs <- xml_attr(xml, "href")
        ids <- str_extract(hrefs, "[0-9]+")
        select <- pages[[i]] - (page - 1) * 100

        result <- c(result, ids[select])
    }

    result <- result[match(unlist(pages), places)]
    result <- as.numeric(result)
    return(result)
}
