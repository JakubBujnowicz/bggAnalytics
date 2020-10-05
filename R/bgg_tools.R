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
    assert_that(inherits(x, "bggAPI"))
    assert_that(inherits(y, "bggAPI"))

    # Assign to avoid NOTEs while checking the package
    Variable <- NULL
    PrettyName <- NULL

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


#' Get IDs of top rated games on BGG
#'
#' This function scraps BGG website for IDs of games with given \code{places}
#' in the games ranking.
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
    assert_that(.are_positive_integers(places))

    pages <- ceiling(places / 100)
    pages <- split(places, pages)

    page_no <- as.numeric(names(pages))

    result <- numeric()
    for (i in seq_along(pages)) {
        page <- page_no[i]

        xml <- read_html(paste0(.bgg_url("ranking"), "/page/", page))
        xml <- xml_nodes(xml, xpath = "//*[@class = 'primary']")

        hrefs <- xml_attr(xml, "href")
        ids <- as.numeric(str_extract(hrefs, "[0-9]+"))
        select <- pages[[i]] - (page - 1) * 100

        result <- c(result, ids[select])
    }

    result <- result[match(unlist(pages), places)]
    return(result)
}


#' Get BGG URLs for games with given IDs
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
    assert_that(.are_positive_integers(ids))

    result <- paste0(.bgg_url("boardgame"), ids)
    return (result)
}
