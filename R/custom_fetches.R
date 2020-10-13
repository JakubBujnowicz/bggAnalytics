#' Custom fetching methods for non-scalar variables
#'
#' This methods are written for more complicated structures (called variables
#' here) that can't be contained into a simple atomic vector. They are called by
#' name within \code{.fetch_internal}.
#'
#' @param xml an XML nodeset.
#'
#' @name custom_fetches
#'
#' @return Type of output might be different for every variable.
#' @keywords internal
#'
NULL

#' @describeIn custom_fetches Method for pollplayers of bggGames.
#'
.fetch_pollplayers <- function(xml)
{
    polls <- xml_find_first(xml, xpath = "poll[@name = 'suggested_numplayers']")
    polls <- lapply(polls, xml_find_all, xpath = "results")

    numplayers <- lapply(polls, .attr2text, xpath = ".",
                         attr = "numplayers", scalar = TRUE)
    best <- lapply(polls, .attr2number,
                   xpath = "result[@value = 'Best']",
                   attr = "numvotes", scalar = TRUE)
    recommended <- lapply(polls, .attr2number,
                          xpath = "result[@value = 'Recommended']",
                          attr = "numvotes", scalar = TRUE)
    not_recc <- lapply(polls, .attr2number,
                       xpath = "result[@value = 'Not Recommended']",
                       attr = "numvotes", scalar = TRUE)

    result <- list()
    for (i in seq_along(polls)) {
        result[[i]] <- data.table(numplayers = numplayers[[i]],
                                  best = best[[i]],
                                  recommended = recommended[[i]],
                                  notrecommended = not_recc[[i]])
    }

    return(result)
}

#' @describeIn custom_fetches Method for pollage of bggGames.
#'
.fetch_pollage <- function(xml)
{
    polls <- xml_find_first(xml, xpath = "poll[@name = 'suggested_playerage']")
    polls <- lapply(polls, xml_find_all, xpath = "results/result")

    age <- lapply(polls, .attr2text, xpath = ".",
                  attr = "value", scalar = TRUE)
    votes <- lapply(polls, .attr2number, xpath = ".",
                    attr = "numvotes", scalar = TRUE)

    result <- list()
    for (i in seq_along(polls)) {
        result[[i]] <- data.table(age = age[[i]],
                                  votes = votes[[i]])
    }

    return(result)
}

#' @describeIn custom_fetches Method for polllanguage of bggGames.
#'
.fetch_polllanguage <- function(xml)
{
    polls <- xml_find_first(xml, xpath = "poll[@name = 'language_dependence']")
    polls <- lapply(polls, xml_find_all, xpath = "results/result")

    description <- lapply(polls, .attr2text, xpath = ".",
                          attr = "value", scalar = TRUE)
    level <- lapply(polls, .attr2number, xpath = ".",
                    attr = "level", scalar = TRUE)
    votes <- lapply(polls, .attr2number, xpath = ".",
                    attr = "numvotes", scalar = TRUE)

    result <- list()
    for (i in seq_along(polls)) {
        result[[i]] <- data.table(level = level[[i]],
                                  description = description[[i]],
                                  votes = votes[[i]])
    }

    return(result)
}

#' @describeIn custom_fetches Method for bestplayers of bggGames.
#'
.fetch_bestplayers <- function(xml) .playerpoll_outcome(xml, "best")
#' @describeIn custom_fetches Method for recplayers of bggGames.
#'
.fetch_recplayers <- function(xml) .playerpoll_outcome(xml, "recommended")
#' @describeIn custom_fetches Method for bestplayers of bggGames.
#'
.fetch_notrecplayers <- function(xml) .playerpoll_outcome(xml, "notrecommended")

# Multipurpose #################################################################
.playerpoll_outcome <- function(xml, category)
{
    # Assertions
    types <- c("best", "recommended", "notrecommended")
    assert_that(.is_string(category, allowed = types))

    res <- .fetch_pollplayers(xml)

    ids <- rep(seq_along(res), sapply(res, nrow))
    res <- rbindlist(res)
    res[, "i" := ids]
    res[, "most" := pmax(res$best, res$recommended, res$notrecommended)]

    res[, "outcome" := fcase(is.na(res$most) | res$most == 0, FALSE,
                            get(category) == res$most,   TRUE,
                            default = FALSE)]

    res <- res[, list(outcome = get("numplayers")[get("outcome")]), by = "i"]
    res <- split(res$outcome, factor(res$i, levels = seq_along(xml)))
    res <- unname(res)
    return(res)
}
