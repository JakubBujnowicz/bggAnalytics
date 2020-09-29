bgg_merge <- function(x, y, ...)
{
    # Assertions
    assert_that(is.data.table(x))
    assert_that(is.data.table(y))

    # Get cols from y that are missing in x
    y_cols <- c("objectid", setdiff(names(y), names(x)))
    y <- y[, y_cols, with = FALSE]

    result <- merge(x, y,
                    by = "objectid",
                    all = FALSE,
                    sort = FALSE)
    return(result)
}

bgg_topgames <- function(n = 100)
{
    # Assertions
    assert_that(.is_positive_integer(n))

    k <- ceiling(n / 100)

    result <- list()
    for (i in seq_len(k)) {
        xml <- read_html(paste0(.bgg_url("ranking"), "/page/", i))
        xml <- xml_nodes(xml, xpath = "//*[@class = 'primary']")

        hrefs <- xml_attr(xml, "href")
        result[[i]] <- as.numeric(str_extract(hrefs, "[0-9]+"))
    }

    result <- unlist(result)[seq_len(n)]
    return(result)
}

bgg_gameurl <- function(ids)
{
    # Assertions
    assert_that(.are_positive_integers(ids))

    result <- paste0(.bgg_url("boardgame"), ids)
    return (result)
}
