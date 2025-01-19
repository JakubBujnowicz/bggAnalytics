# Definition ###################################################################
#' API for user collections
#'
#' @description Access the data of a given user's collection. See
#'   \code{\link{bggAPI}} for more details on inherited slots and methods.
#'
#' @references
#'   [BoardGameGeek XML API2](https://boardgamegeek.com/wiki/page/BGG_XML_API2)
#'
#' @export
#' @include bggAPI.R
#'
bggCollection <- R6Class(
    classname = "bggCollection",
    inherit = bggAPI,
    private = list(
        # Fields
        .username = character()
    ),

    active = list(
        #' @field username A single string, name of a user whose collection
        #'   should be fetched.
        username = .private_getter("username")
    ),

    public = list(
    # Initialize ---------------------------------------------------------------
    #' @description Object initialization.
    #'
    #' @param username a single string with a BoardGameGeek name of a user whose
    #'   collection is to be fetched.
    #' @param params a list of object parameters. If not all the parameters are
    #'   included in the list, default values are used (`NULL` instead of
    #'   the list is possible for all the default parameters). \cr
    #'   Following parameters are allowed for the `bggGames` class with
    #'   default values in parentheses:
    #'   \itemize{
    #'       \item{`pretty_names`} - (`FALSE`) a boolean value,
    #'       should the object should use pretty names,
    #'       \item{`stats`} - (`TRUE`) a boolean value, should the
    #'       ranking and rating stats be included for every item. Note that some
    #'       variables require that `stats` is `TRUE`.
    #'       \item{`brief`} - (`FALSE`) a boolean value, should the
    #'       results be abbreviated.
    #'       \item{`own, rated, played, comment, trade, want, wishlist`} -
    #'       (`NULL`) a boolean value, `FALSE` excludes items with a
    #'       given status while `TRUE` includes only them. `NULL`
    #'       returns items regardless of the status.
    #'       \item{`wishlistpriority`} - (`NULL`) a positive integer
    #'       between 1 and 5, returns only items with a given wishlist
    #'       priority. `NULL` returns items regardless of the priority.
    #'       \item{`minrating, rating`} - (`NULL`) a positive integer
    #'       between 1 and 10, returns only items with a given minimum rating
    #'       (`minrating`) or maximum rating (`rating`). `NULL`
    #'       returns items regardless of the rating.
    #'   }
    initialize = function(username = NULL, params = NULL)
    {
        if (is.null(username)) {
            username <- getOption(".bggAnalytics.username")
        }

        # Assertions -----------------------------------------------------------
        assert_string(username)
        params <- .process_params(params, class = "bggCollection")

        # Connecting to API ----------------------------------------------------
        api_url <- paste0(.bgg_url("api"), "collection?username=", username)
        api_url <- .extend_url_by_params(api_url, params,
                                         class = "bggCollection")

        # Check if the request has been processed
        xml <- read_xml(api_url)
        txt <- xml_text(xml)

        processing_message <-
            "request for this collection has been accepted and will be processed."

        messages <- getOption(".bggAnalytics.verbose")
        while (grepl(processing_message, txt)) {
            if (messages) {
                message("Server needs time to process the request...")
                messages <- FALSE
            }

            # Server needs a while to process this request
            Sys.sleep(1)

            # Try again
            xml <- read_xml(api_url)
            txt <- xml_text(xml)
        }
        xml <- .xml_expand(xml)

        # Extract IDs ----------------------------------------------------------
        ids <- as.numeric(xml_attr(xml, attr = "objectid"))

        if (length(ids) == 0) {
            warning("this collection contains no games, perhaps the ",
                    "username is wrong?")
        }

        # Sorting IDs and XML
        ids_order <- order(ids)
        ids <- ids[ids_order]
        xml <- xml[ids_order]

        # Setting private variables --------------------------------------------
        private$.timestamp <- Sys.time()
        private$.username <- username
        private$.ids <- ids
        private$.xml <- xml
        private$.api_url <- api_url
        private$.params <- params
        private$.data <- data.table(objectid = ids)
        setkey(private$.data, objectid)

        if (params$pretty_names) {
            self$switch_namestyle("pretty")
        }
    },

    # Print --------------------------------------------------------------------
    #' @description Print object information.
    #'
    print = function()
    {
        n_show <- getOption(".bggAnalytics.print")

        nc <- ncol(private$.data)
        nr <- nrow(private$.data)

        string <- paste0(
            "----- bggCollection -----",
            "\nUser collection API of the following user: '", private$.username,
            "'.\nCreation timestamp: ", private$.timestamp,
            ".\nThe data contains ", nr, " ", .plural("object", nr), " and ",
            nc, " ", .plural("variable", nc), ".\n\n")
        cat(string)
        cat("--------- Data ----------\n")
        print(private$.data, nrows = n_show, trunc.cols = TRUE)
    })
)
