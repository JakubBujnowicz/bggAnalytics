#' Return a given BGG URL
#'
#' Get hyperlinks to given pages of BoardGameGeek site.
#'
#' @param of Single string, either "api" or "boardgame".
#'
#' @return Single string with a page URL.
#' @keywords internal
#'
bgg_url <- function(of) {
    assert_that(is.string(of))

    result <- switch(of,
                     api = "https://boardgamegeek.com/xmlapi2/",
                     boardgame = "https://boardgamegeek.com/boardgame/",
                     NA)
    if (is.na(result)) {
        stop(of, " is not a proper 'of' value")
    }

    return(result)
}


#' Get all 'items' nodes from XML Nodeset
#'
#' This function expands xml_nodeset to all 'item' children. It allows to use
#' `lapply(., f)` per Game ID.
#'
#' @param xml XML Nodeset.
#'
#' @return XML Nodeset.
#' @keywords internal
#'
xml_expand <- function(xml) {
    result <- html_node(xml, "items") %>%
        xml_children()
    return (result)
}


nodes2text <- function(xml, xpath) {
    assert_that(is.string(xpath), noNA(xpath))

    nodes <- lapply(xml, html_nodes, xpath = xpath)
    values <- lapply(nodes, html_text, trim = TRUE)

    return (values)
}

nodes2number <- function(xml, xpath) {
    assert_that(is.string(xpath), noNA(xpath))

    nodes <- lapply(xml, html_nodes, xpath = xpath)
    values <- lapply(nodes, xml_double)

    return (values)
}

attr2text <- function(xml, xpath, attr) {
    assert_that(is.string(xpath), noNA(xpath))
    assert_that(is.string(attr), noNA(attr))

    nodes <- lapply(xml, html_nodes, xpath = xpath)
    values <- lapply(nodes, xml_attr, attr = attr)

    return(values)
}

attr2number <- function(xml, xpath, attr) {
    # Assertions for xpath and attr made in attr2text

    values <- attr2text(xml = xml, xpath = xpath, attr = attr)
    values <- lapply(values, as.numeric)

    return(values)
}
