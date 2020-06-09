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
        .usernames = character(),
        .ids = numeric()
    ),
    active = list(
        usernames = private_getter("usernames"),
        ids = private_getter("ids")
    )
)

# Initialize ###################################################################
bggCollection$set("public", "initialize",
function(usernames = NULL,
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
    if (is.null(usernames)) {
        usernames <- getOption(".bggAnalytics.username")

        if (is.null(usernames)) {
            stop("username must be specified, consider setting
                               default username by ",
                 "options(.bggAnalytics.username = 'your_username'")
        }
    }

    # Assertions ---------------------------------------------------------------
    assert_that(is.string(usernames), noNA(usernames))
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
                      paste0(usernames, collapse = ",")) %>%
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

    max_tries <- getOption(".bggAnalytics.max_tries")
    tries <- 1
    while (length(xml) == 0 & tries <= max_tries) {
        cat("Server need time to process the request, trying again in",
            try_delay, "seconds...\t\r")
        # Server needs a while to process this request
        Sys.sleep(tries + 1)

        # Try again
        xml <- read_html(api_url)

        # Increment a little bit
        tries <- tries + 1
    }
    xml <- xml_expand(xml)

    # Setting params -----------------------------------------------------------
    ids <- as.numeric(sapply(xml, xml_attr, attr = "objectid"))

    private$.usernames <- usernames
    private$.ids <- ids
    private$.xml <- xml
    private$.data <- data.table(objectid = ids)
})


# Print ########################################################################
bggCollection$set("public", "print",
function() {
    n_show <- getOption(".bggAnalytics.print")

    string <- paste0(
        "---- bggCollection ----",
        "\nUser collection data API.\n",
        "\n* Usernames: ", compress(private$.usernames,
                                    n_show = n_show),
        "\n* IDs: ", compress(private$.ids,
                              n_show = n_show),
        "\n* Variables: ", compress(names(private$.data),
                                    n_show = n_show))

    cat(string)
})
