# Documentation ################################################################
#' @title Generalised Distribution Object
#'
#' @description A generalised distribution object for defining custom probability distributions
#'   as well as serving as the parent class to specific, familiar distributions. Common
#'   mathematical and statistical methods for distributions are defined here with approximate numerical
#'   calculations (as opposed to analytical results).
#'
#' @name bggCollection
#'
#' @export
#'
NULL

# Definition ###################################################################
bggCollection <- R6Class(
    classname = "bggCollection",
    inherit = bggAPI,
    private = list(
        .username = character(),
        .ids = numeric()
    ),
    active = list(
        username = private_getter("username"),
        ids = private_getter("ids")
    )
)

# Initialize ###################################################################
bggCollection$set("public", "initialize",
function(username = NULL,
         stats = FALSE,
         brief = FALSE,
         own = NULL,
         rated = NULL,
         played = NULL,
         comment = NULL,
         trade = NULL,
         want = NULL,
         wishlist = NULL,
         wishlistpriority = NULL,
         minrating = NULL,
         rating = NULL) {

    if (is.null(username)) {
        username <- getOption(".bggAnalytics.username")
    }

    # Assertions ---------------------------------------------------------------
    assert_that(is.string(username), noNA(username))
    # Flags
    assert_that(is.flag(stats), noNA(stats))
    assert_that(is.flag(brief), noNA(brief))
    # Flags with null
    assert_that(is_nullflag(own))
    assert_that(is_nullflag(rated))
    assert_that(is_nullflag(comment))
    assert_that(is_nullflag(trade))
    assert_that(is_nullflag(want))
    assert_that(is_nullflag(wishlist))
    # Value filters
    assert_that(is_nullcount(wishlistpriority, limit = 5))
    assert_that(is_nullcount(minrating, limit = 10))
    assert_that(is_nullcount(rating, limit = 10))

    # Connecting to API --------------------------------------------------------
    api_url <- paste0(bgg_url("api"), "collection?username=",
                      paste0(username, collapse = ",")) %>%
        path_add_flag(stats) %>%
        path_add_flag(brief) %>%
        path_add_filter(own) %>%
        path_add_filter(rated) %>%
        path_add_filter(played) %>%
        path_add_filter(comment) %>%
        path_add_filter(trade) %>%
        path_add_filter(want) %>%
        path_add_filter(wishlist) %>%
        path_add_filter(wishlistpriority) %>%
        path_add_filter(minrating) %>%
        path_add_filter(rating)

    xml <- read_html(api_url)

    # Check if the request has been processed
    txt <- xml_text(xml)
    processing_message <-
        "request for this collection has been accepted and will be processed."
    messages <- getOption(".bggAnalytics.verbose")
    while (xml_length(xml) == 1 && grepl(processing_message, txt)) {
        if (messages) {
            message("Server needs time to process the request...")
            messages <- FALSE
        }

        # Server needs a while to process this request
        Sys.sleep(1)

        # Try again
        xml <- read_html(api_url)
        txt <- xml_text(xml)
    }
    xml <- xml_expand(xml)

    # Setting params -----------------------------------------------------------
    ids <- as.numeric(sapply(xml, xml_attr, attr = "objectid"))

    if (length(ids) == 0) {
        warning("this collection contains no games, perhaps the username is wrong?")
    }

    private$.username <- username
    private$.ids <- ids
    private$.xml <- xml
    private$.api_url <- api_url
    private$.data <- data.table(objectid = ids, key = "objectid")
    private$.params <- list(stats = stats,
                            brief = brief,
                            own = own,
                            rated = rated,
                            played = played,
                            comment = comment,
                            trade = trade,
                            want = want,
                            wishlist = wishlist,
                            wishlistpriority = wishlistpriority,
                            minrating = minrating,
                            rating = rating)
})


# Print ########################################################################
bggCollection$set("public", "print",
function() {
    n_show <- getOption(".bggAnalytics.print")

    string <- paste0(
        "---- bggCollection ----",
        "\nUser collection data API.\n",
        "\n* username: ", compress(private$.username,
                                    n_show = n_show),
        "\n* IDs: ", compress(private$.ids,
                              n_show = n_show),
        "\n* Variables: ", compress(names(private$.data),
                                    n_show = n_show))

    cat(string)
})
