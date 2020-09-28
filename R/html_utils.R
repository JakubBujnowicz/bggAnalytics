#' Return a given BGG URL
#'
#' Get hyperlinks to given pages of BoardGameGeek site.
#'
#' @param of Single string, either "api", "ranking" or "boardgame".
#'
#' @return Single string with a page URL.
#' @keywords internal
#'
.bgg_url <- function(of)
{
    result <- switch(of,
                     api = "https://boardgamegeek.com/xmlapi2/",
                     boardgame = "https://boardgamegeek.com/boardgame/",
                     ranking = "https://boardgamegeek.com/browse/boardgame",
                     NA)
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
.xml_expand <- function(xml)
{
    result <- html_node(xml, "items") %>%
        xml_children()
    return (result)
}

# Extraction ###################################################################
#' Extraction functions
#'
#' These functions extract data from given nodes/attributes from XML item list.
#'
#' @param xml XML item list.
#' @param xpath Single string, name of XPATH expression, for example node name.
#' @param attr Single string, name of attribute
#'
#' @return List of elements of equal length as \code{xml}.
#' @keywords internal
#'
#' @name extraction_functions
#'
NULL

#' @rdname extraction_functions
.nodes2text <- function(xml, xpath)
{
    nodes <- html_node(xml, xpath = xpath)
    values <- html_text(nodes, trim = TRUE)

    return (values)
}

#' @rdname extraction_functions
.nodes2number <- function(xml, xpath)
{
    nodes <- html_node(xml, xpath = xpath)
    values <- suppressWarnings(xml_double(nodes))

    return (values)
}

#' @rdname extraction_functions
.nodes2logical <- function(xml, xpath)
{
    values <- .nodes2number(xml, xpath = xpath)
    values <- as.logical(values)

    return (values)
}

#' @rdname extraction_functions
.attr2text <- function(xml, xpath, attr)
{
    nodes <- html_node(xml, xpath = xpath)
    values <- xml_attr(nodes, attr = attr)

    return(values)
}

#' @rdname extraction_functions
.attr2number <- function(xml, xpath, attr)
{
    values <- .attr2text(xml = xml, xpath = xpath, attr = attr)
    values <- suppressWarnings(as.numeric(values))

    return(values)
}

#' @rdname extraction_functions
.attr2logical <- function(xml, xpath, attr)
{
    values <- .attr2number(xml = xml, xpath = xpath, attr = attr)
    values <- as.logical(values)

    return(values)
}
