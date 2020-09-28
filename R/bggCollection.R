# Definition ###################################################################
bggCollection <- R6Class(
    classname = "bggCollection",
    inherit = bggAPI,
    private = list(
        .username = character(),
        .ids = numeric(),
        .bggGames = NULL
    ),
    active = list(
        username = .private_getter("username"),
        ids = .private_getter("ids")
    )
)

# Initialize ###################################################################
bggCollection$set("public", "initialize",
function(username = NULL, params = NULL) {

    if (is.null(username)) {
        username <- getOption(".bggAnalytics.username")
    }

    # Assertions ---------------------------------------------------------------
    assert_that(.is_string(username))
    params <- .process_params(params, class = "bggCollection")

    # Connecting to API --------------------------------------------------------
    api_url <- paste0(.bgg_url("api"), "collection?username=", username)
    api_url <- .extend_url_by_params(api_url, params, class = "bggCollection")

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
    xml <- .xml_expand(xml)

    # Extract IDs --------------------------------------------------------------
    ids <- as.numeric(sapply(xml, xml_attr, attr = "objectid"))

    if (length(ids) == 0) {
        warning("this collection contains no games, perhaps the username is wrong?")
    }

    # Setting private variables ------------------------------------------------
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
})


# Print ########################################################################
bggCollection$set("public", "print",
function()
{
    n_show <- getOption(".bggAnalytics.print")

    nc <- ncol(private$.data)
    nr <- nrow(private$.data)

    string <- paste0(
        "---- bggCollection ----",
        "\nUser collection API of the following user: '", private$.username,
        "'.\nThe data contains ", nr, " ", .plural("object", nr), " and ",
        nc, " ", .plural("variable", nc), ".\n\n")
    cat(string)
    print(private$.data, nrows = n_show, trunc.cols = TRUE)
})

