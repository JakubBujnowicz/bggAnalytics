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
    assert_that(is.character(of), is.scalar(of))

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
expand_xml <- function(xml) {
    result <- xml_children(html_node(xml, "items"))
    return (result)
}
