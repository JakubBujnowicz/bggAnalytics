# Definition ###################################################################
bggGames <- R6Class(
    classname = "bggGames",
    inherit = bggAPI,
    private = list(
        .ids = numeric()
    ),
    active = list(
        ids = private_getter("ids")
    )
)

# Initialize ###################################################################
bggGames$set("public", "initialize",
function(ids, stats = TRUE) {
    # Assertions
    assert_that(is.numeric(ids), not_empty(ids),
                noNA(ids), all(ids %% 1 == 0), all(ids > 0))
    assert_that(is.flag(stats), noNA(stats))

    ids <- unique(ids)

    # Getting full URL with params
    api_url <- paste0(bgg_url("api"), "thing?id=",
                      paste0(ids, collapse = ",")) %>%
        path_add_flag(stats)

    xml <- read_html(api_url) %>%
        xml_expand

    xml_ids <- as.numeric(sapply(xml, xml_attr, attr = "id"))
    missing <- setdiff(ids, xml_ids)

    # Check for any success
    ids <- intersect(ids, xml_ids)
    if (length(ids) == 0) {
        stop("No ids were available through BGG API", call. = FALSE)
    }

    # Check for missing
    if (length(missing) > 0) {
        warning("Following ids were not available through BGG API:\n",
                paste0(missing, collapse = ", "), call. = FALSE)
    }

    private$.ids <- ids
    private$.xml <- xml
    private$.api_url <- api_url
    private$.data <- data.table(objectid = ids, key = "objectid")
    private$.params <- list(stats = stats)
})


# Print ########################################################################
bggGames$set("public", "print",
function() {
    n_show <- getOption(".bggAnalytics.print")

    string <- paste0(
        "---- bggGames ----",
        "\nGames data API.\n",
        "\n* IDs: ", compress(private$.ids,
                              n_show = n_show),
        "\n* Variables: ", compress(names(private$.data),
                                    n_show = n_show))

    cat(string)
})
