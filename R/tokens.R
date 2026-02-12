#' BGG Authorization tokens
#'
#' Required to handle most requests for BGG XML API2. For more information
#' please see "Using the XML API"'s section "Application Tokens".
#'
#' @param token a single string, the authorization token.
#'
#' @references
#' [Using the XML API](https://boardgamegeek.com/using_the_xml_api)
#'
#' @export
#'
#' @examples
#' \dontrun{
#'     # Token taken from the example in the link above, just for reference
#'     set_token("e3f8c3ff-9926-4efc-863c-3b92acda4d32")
#'
#'     get_token()
#' }
#'
#' @name tokens
#'
set_token <- function(token)
{
    token_regex <- "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
    token <- tolower(token)
    checkmate::assert_string(token, pattern = token_regex)

    # Setting the option
    options(bggAnalytics.token = token)
    message("authentication token succesfully set")

    return(invisible(token))
}


#' @rdname tokens
#' @export
#'
get_token <- function()
{
    token <- getOption("bggAnalytics.token")
    if (is.null(token)) {
        stop("the authentication token has not been set yet, ",
             "please use set_token() function")
    }

    return(token)
}


