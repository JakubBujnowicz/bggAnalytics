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
    lng_polls <- .xml_concatenate(polls)

    numplayers <- .attr2text(lng_polls,
                             xpath = ".",
                             attr = "numplayers",
                             scalar = TRUE)
    best <- .attr2number(lng_polls,
                         xpath = "result[@value = 'Best']",
                         attr = "numvotes",
                         scalar = TRUE)
    recommended <- .attr2number(lng_polls,
                                xpath = "result[@value = 'Recommended']",
                                attr = "numvotes",
                                scalar = TRUE)
    not_recc <- .attr2number(lng_polls,
                             xpath = "result[@value = 'Not Recommended']",
                             attr = "numvotes",
                             scalar = TRUE)

    # Split result back by IDs
    result <- data.table(numplayers = numplayers,
                         best = best,
                         recommended = recommended,
                         notrecommended = not_recc)
    result <- split(result, rep(seq_along(polls), lengths(polls)))

    return(result)
}

#' @describeIn custom_fetches Method for pollage of bggGames.
#'
.fetch_pollage <- function(xml)
{
    polls <- xml_find_first(xml, xpath = "poll[@name = 'suggested_playerage']")
    polls <- lapply(polls, xml_find_all, xpath = "results/result")
    lng_polls <- .xml_concatenate(polls)

    age <- .attr2text(lng_polls,
                      xpath = ".",
                      attr = "value",
                      scalar = TRUE)
    votes <- .attr2number(lng_polls,
                          xpath = ".",
                          attr = "numvotes",
                          scalar = TRUE)

    # Split results by IDs
    result <- data.table(age = age,
                         votes = votes)
    result <- split(result, rep(seq_along(polls), lengths(polls)))

    return(result)
}

#' @describeIn custom_fetches Method for polllanguage of bggGames.
#'
.fetch_polllanguage <- function(xml)
{
    polls <- xml_find_first(xml, xpath = "poll[@name = 'language_dependence']")
    polls <- lapply(polls, xml_find_all, xpath = "results/result")
    lng_polls <- .xml_concatenate(polls)

    description <- .attr2text(lng_polls,
                              xpath = ".",
                              attr = "value",
                              scalar = TRUE)
    level <- .attr2number(lng_polls,
                          xpath = ".",
                          attr = "level",
                          scalar = TRUE)
    votes <- .attr2number(lng_polls,
                          xpath = ".",
                          attr = "numvotes",
                          scalar = TRUE)

    # Split results by IDs
    result <- data.table(level = level,
                         description = description,
                         votes = votes)
    result <- split(result, rep(seq_along(polls), lengths(polls)))

    return(result)
}

#' @describeIn custom_fetches Method for ranks of bggGames.
#'
.fetch_ranks_gms <- function(xml)
    .fetch_ranks(xml, "statistics/ratings/ranks")

#' @describeIn custom_fetches Method for ranks of bggCollection.
#'
.fetch_ranks_cllctn <- function(xml)
    .fetch_ranks(xml, "stats/rating/ranks")

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

.fetch_ranks <- function(xml, ranks_xpath)
{
    ranks <- xml_find_first(xml, xpath = ranks_xpath)
    tabs <- lapply(ranks, function(x) xml_attrs(
        xml_find_all(x, xpath = "./rank")))

    # Assignment to avoid NOTEs while checking the package
    friendlyname <- NULL
    value <- NULL
    bayesaverage <- NULL

    # Join all ranks into one data.table
    result <- rbindlist(lapply(unlist(tabs, recursive = FALSE), as.list))
    result[, ":="(friendlyname = str_remove(friendlyname, " Rank$"),
                  value = as.numeric(
                      fifelse(value == "Not Ranked",
                              NA_character_,
                              value)),
                  bayesaverage = as.numeric(
                      fifelse(bayesaverage == "Not Ranked",
                              NA_character_,
                              bayesaverage)),
                  id = NULL)]

    # Split results per ID once again
    result <- split(result, rep(seq_along(tabs), lengths(tabs)))
    result <- unname(result)
    return(result)
}
