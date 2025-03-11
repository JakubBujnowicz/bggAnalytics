#' Safely scrap HTML website
#'
#' Opens the connection with `curl` and closes in case of error. Returns
#' more unambiguous error messages.
#'
#' @param url a single string, an URL to a given website.
#' @param ... other arguments passed to `read_html`.
#'
#' @return A parsed XML.
#' @keywords internal
#'
.bgg_readurl <- function(url, ...)
{
    assert_string(url)

    .http_error <- function(e)
    {
        close(con)
        ec <- as.character(e)

        has_http_err <- grepl("HTTP error ", ec)
        if (has_http_err) {
            code <- str_extract(ec, "[0-9]+\\.\\n$")
            code <- str_remove(code, "\\.\\n$")
            stop("unable to open the connection with '",
                 url, "' due to the HTTP error ",
                 code, call. = FALSE)
        } else {
            stop("unable to open the connection with '",
                 url, "'", call. = FALSE)
        }
    }

    con <- url(url)

    result <- tryCatch(read_html(con),
                       error = .http_error)
    return(result)
}


#' Return a given BoardGameGeek URL
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
#' This function expands `xml_nodeset` objects to all 'item' children. It
#' allows to use `lapply(., f)` per object (corresponding to one row in
#' `data`).
#'
#' @param xml an XML nodeset.
#'
#' @return XML nodeset.
#' @keywords internal
#'
.xml_expand <- function(xml)
{
    result <- xml_find_all(xml, xpath = "item")
    return(result)
}


#' Concatenate XML nodesets from a list
#'
#' Take a list of XML nodesets (e.g. outputs of `.xml_expand`) and combine
#' them together.
#'
#' @param xml_list a list of XML nodesets.
#'
#' @return An XML nodeset.
#' @keywords internal
#'
.xml_concatenate <- function(xml_list)
{
    for (i in seq_along(xml_list)[-1]) {
        n <- length(xml_list[[1]])
        vec <- seq_along(xml_list[[i]])
        xml_list[[1]][n + vec] <- xml_list[[i]]
    }

    return(xml_list[[1]])
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
#' @return List of elements of equal length as `xml`.
#' @keywords internal
#'
#' @name extraction_functions
#'
NULL

#' @rdname extraction_functions
.nodes2text <- function(xml, xpath, scalar = TRUE)
{
    if (scalar) {
        nodes <- xml_find_first(xml, xpath = xpath)
        values <- xml_text(nodes, trim = TRUE)
    } else {
        nodes <- lapply(xml, xml_find_all, xpath = xpath)
        values <- lapply(nodes, xml_text, trim = TRUE)
    }

    return(values)
}

#' @rdname extraction_functions
.nodes2number <- function(xml, xpath, scalar = TRUE)
{
    if (scalar) {
        nodes <- xml_find_first(xml, xpath = xpath)
        values <- suppressWarnings(xml_double(nodes))
    } else {
        nodes <- lapply(xml, xml_find_all, xpath = xpath)
        values <- suppressWarnings(lapply(nodes, xml_double))
    }

    return(values)
}

#' @rdname extraction_functions
.nodes2logical <- function(xml, xpath, scalar = TRUE)
{
    if (scalar) {
        nodes <- xml_find_first(xml, xpath = xpath)
        values <- xml_text(nodes, trim = TRUE) == "1"
    } else {
        nodes <- lapply(xml, xml_find_all, xpath = xpath)
        values <- lapply(nodes, function(x) xml_text(x, trim = TRUE) == "1")
    }

    return(values)
}

#' @rdname extraction_functions
.attr2text <- function(xml, xpath, attr, scalar = TRUE)
{
    if (scalar) {
        nodes <- xml_find_first(xml, xpath = xpath)
        values <- xml_attr(nodes, attr = attr)
    } else {
        nodes <- lapply(xml, xml_find_all, xpath = xpath)
        values <- lapply(nodes, xml_attr, attr = attr)
    }

    return(values)
}

#' @rdname extraction_functions
.attr2number <- function(xml, xpath, attr, scalar = TRUE)
{
    if (scalar) {
        nodes <- xml_find_first(xml, xpath = xpath)
        values <- xml_attr(nodes, attr = attr)
        values <- suppressWarnings(as.numeric(values))
    } else {
        nodes <- lapply(xml, xml_find_all, xpath = xpath)
        values <- lapply(nodes, function(x) as.numeric(xml_attr(x, attr = attr)))
    }

    return(values)
}

#' @rdname extraction_functions
.attr2logical <- function(xml, xpath, attr, scalar = TRUE)
{
    if (scalar) {
        nodes <- xml_find_first(xml, xpath = xpath)
        values <- xml_attr(nodes, attr = attr) == "1"
    } else {
        nodes <- lapply(xml, xml_find_all, xpath = xpath)
        values <- lapply(nodes, function(x) xml_attr(x, attr = attr) == "1")
    }

    return(values)
}

#' @rdname extraction_functions
.attr2datetime <- function(xml, xpath, attr, scalar = TRUE)
{
    if (scalar) {
        nodes <- xml_find_first(xml, xpath = xpath)
        values <- as.POSIXct(xml_attr(nodes, attr = attr))
    } else {
        nodes <- lapply(xml, xml_find_all, xpath = xpath)
        values <- lapply(nodes, function(x) as.POSIXct(xml_attr(x, attr = attr)))
    }

    return(values)
}
