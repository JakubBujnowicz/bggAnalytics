# Definition ###################################################################
bggGames <- R6Class(
    classname = "bggGames",
    inherit = bggAPI,
    private = list(
        .ids = numeric()
    ),
    active = list(
        ids = .private_getter("ids")
    )
)

# Initialize ###################################################################
bggGames$set("public", "initialize",
function(ids, params = NULL) {
    # Assertions ---------------------------------------------------------------
    assert_that(.are_positive_integers(ids))
    params <- .process_params(params, class = "bggGames")

    ids <- unique(ids)

    # Getting the API URL and XML
    api_url <- paste0(.bgg_url("api"), "thing?id=",
                      paste0(ids, collapse = ","))
    api_url <- .extend_url_by_params(api_url, params, class = "bggGames")
    xml <- read_html(api_url) %>%
        .xml_expand()

    # Testing IDs
    xml_ids <- as.numeric(sapply(xml, xml_attr, attr = "id"))

    # Check for any success
    ids <- intersect(ids, xml_ids)
    if (length(ids) == 0) {
        stop("None of the given 'ids' were available through BGG API",
             call. = FALSE)
    }

    # Check for missing
    missing <- setdiff(ids, xml_ids)
    if (length(missing) > 0) {
        ids <- setdiff(ids, missing)
        warning("Following ids were not available through BGG API:\n",
                toString(missing), call. = FALSE)
    }

    # Setting private variables ------------------------------------------------
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
bggGames$set("public", "print",
function()
{
    n_show <- getOption(".bggAnalytics.print")

    nc <- ncol(private$.data)
    nr <- nrow(private$.data)

    string <- paste0(
        "---- bggGames ----",
        "\nGames data API.\n",
        "The data contains ", nr, " ", .plural("object", nr), " and ",
        nc, " ", .plural("variable", nc), ".\n\n")
    cat(string)
    print(private$.data, nrows = n_show, trunc.cols = TRUE)
})
