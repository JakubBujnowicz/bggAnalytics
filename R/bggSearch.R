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

    query_str <- gsub("[[:space:]]", "%20", query)
    query_str <- paste0(query_str, collapse = "%20")

    api_url <- paste0(.bgg_url("api"), "search?query=", query_str)
    api_url <- .extend_url_by_params(api_url, params, class = "bggSearch")

    xml <- read_html(api_url) %>%
        .xml_expand()

    ids <- as.numeric(sapply(xml, xml_attr, attr = "id"))

    var_specs <- var_specs[Class == "bggSearch"]
    data <- .fetch_internal(xml, var_specs$Variable, var_specs)
    data <- data.table(objectid = ids, as.data.table(data))
    duplicates <- duplicated(data)

    private$.query <- query
    private$.ids <- ids[!duplicates]
    private$.xml <- xml[!duplicates]
    private$.api_url <- api_url
    private$.data <- data[!duplicates]
    private$.params <- params
})


# Print ########################################################################
bggSearch$set("public", "print",
function() {
    n_show <- getOption(".bggAnalytics.print")
    n <- nrow(private$.data)

    if (n <= n_show) {
        result_str <- "Full result table:\n"
    } else {
        result_str <- paste0("Showing first ", n_show, " results:\n")
    }

    string <- paste0(
        "---- bggSearch ----",
        "\nSearch API with the following query: '",
            paste0(private$.query, collapse = " "), "'.\n",
        "Found ", n, " results.\n\n",
        result_str)
    cat(string)
    print(head(private$.data, n_show))
})
