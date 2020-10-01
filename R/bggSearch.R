# Definition ###################################################################
bggSearch <- R6Class(
    classname = "bggSearch",
    inherit = bggAPI,
    private = list(
        .query = character(),
        .ids = numeric()
    ),
    active = list(
        query = .private_getter("query"),
        ids = .private_getter("ids")
    )
)

# Initialize ###################################################################
bggSearch$set("public", "initialize",
function(query, params = NULL) {
    # Assertions
    assert_that(.are_strings(query))
    params <- .process_params(params, class = "bggSearch")

    # Connecting to API --------------------------------------------------------
    query_str <- gsub("[[:space:]]", "%20", query)
    query_str <- paste0(query_str, collapse = "%20")

    api_url <- paste0(.bgg_url("api"), "search?query=", query_str)
    api_url <- .extend_url_by_params(api_url, params, class = "bggSearch")

    xml <- read_html(api_url) %>%
        .xml_expand()


    # Preparing data -----------------------------------------------------------
    ids <- as.numeric(sapply(xml, xml_attr, attr = "id"))
    uniq <- !duplicated(ids)
    # uniq <- rep(TRUE, length(ids))
    data <- data.table(objectid = ids[uniq])
    setkey(data, objectid)


    # Setting private variables ------------------------------------------------
    private$.timestamp <- Sys.time()
    private$.query <- query
    private$.api_url <- api_url
    private$.ids <- ids[uniq]
    private$.xml <- xml[uniq]
    private$.data <- data
    private$.params <- params

    if (params$pretty_names) {
        self$switch_namestyle("pretty")
    }

    self$expand(c("name", "yearpublished"))
})


# Print ########################################################################
bggSearch$set("public", "print",
function()
{
    n_show <- getOption(".bggAnalytics.print")

    nc <- ncol(private$.data)
    nr <- nrow(private$.data)

    string <- paste0(
        "----- bggSearch -----",
        "\nSearch API with the following query: '",
        paste0(private$.query, collapse = " "), "'.\n",
        "Creation timestamp: ", private$.timestamp, ".\n",
        "The data contains ", nr, " ", .plural("object", nr), " and ",
        nc, " ", .plural("variable", nc), ".\n\n")
    cat(string)
    cat("------- Data --------\n")
    print(private$.data, nrows = n_show, trunc.cols = TRUE)
})
