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
#' This function expands \code{xml_nodeset} objects to all 'item' children. It
#' allows to use \code{lapply(., f)} per object (corresponding to one row in
#' \code{data}).
#'
#' @param xml an XML nodeset.
#'
#' @return XML nodeset.
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
.nodes2text <- function(xml, xpath, scalar = TRUE)
{
    if (scalar) {
        nodes <- html_node(xml, xpath = xpath)
        values <- html_text(nodes, trim = TRUE)
    } else {
        nodes <- lapply(xml, html_nodes, xpath = xpath)
        values <- lapply(nodes, html_text, trim = TRUE)
    }

    return (values)
}

#' @rdname extraction_functions
.nodes2number <- function(xml, xpath, scalar = TRUE)
{
    if (scalar) {
        nodes <- html_node(xml, xpath = xpath)
        values <- suppressWarnings(xml_double(nodes))
    } else {
        nodes <- lapply(xml, html_nodes, xpath = xpath)
        values <- suppressWarnings(lapply(nodes, xml_double))
    }

    return (values)
}

#' @rdname extraction_functions
.nodes2logical <- function(xml, xpath, scalar = TRUE)
{
    if (scalar) {
        values <- .nodes2number(xml, xpath = xpath, scalar = scalar)
        values <- as.logical(values)
    } else {
        values <- .nodes2number(xml, xpath = xpath, scalar = scalar)
        values <- lapply(values, as.logical)
    }

    return (values)
}

#' @rdname extraction_functions
.attr2text <- function(xml, xpath, attr, scalar = TRUE)
{
    if (scalar) {
        nodes <- html_node(xml, xpath = xpath)
        values <- xml_attr(nodes, attr = attr)
    } else {
        nodes <- lapply(xml, html_nodes, xpath = xpath)
        values <- lapply(nodes, xml_attr, attr = attr)
    }

    return(values)
}

#' @rdname extraction_functions
.attr2number <- function(xml, xpath, attr, scalar = TRUE)
{
    if (scalar) {
        values <- .attr2text(xml = xml, xpath = xpath, attr = attr,
                             scalar = scalar)
        values <- suppressWarnings(as.numeric(values))
    } else {
        values <- .attr2text(xml = xml, xpath = xpath, attr = attr,
                             scalar = scalar)
        values <- suppressWarnings(lapply(values, as.numeric))
    }

    return(values)
}

#' @rdname extraction_functions
.attr2logical <- function(xml, xpath, attr, scalar = TRUE)
{
    if (scalar) {
        values <- .attr2number(xml = xml, xpath = xpath, attr = attr,
                               scalar = scalar)
        values <- as.logical(values)
    } else {
        values <- .attr2number(xml = xml, xpath = xpath, attr = attr,
                               scalar = scalar)
        values <- lapply(values, as.logical)
    }

    return(values)
}
