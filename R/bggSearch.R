# Definition ###################################################################
#' API for BGG search engine
#'
#' @description Search for items on the BGG with a given query. See
#'   \code{\link{bggAPI}} for more details on inherited slots and methods.
#'
#' @details Note that the result is trimmed to items with unique IDs. Due to
#' XML API2 `Search` returning multiple items for a single ID with different
#' types, variable \code{'type'} might be not accurately represented.
#'
#' @references \href{https://boardgamegeek.com/wiki/page/BGG_XML_API2}{BGG XML
#' API2}
#'
#' @export
#' @include bggAPI.R
#'
bggSearch <- R6Class(
    classname = "bggSearch",
    inherit = bggAPI,
    private = list(
        # Fields
        .query = character()
    ),

    active = list(
        #' @field query A single string with the wanted query.
        query = .private_getter("query")
    ),

    public = list(
    # Initialize ---------------------------------------------------------------
    #' @description Object initialization.
    #'
    #' @param query a single string, query used to perform the search.
    #' @param params a list of object parameters. If not all the parameters are
    #'   included in the list, default values are used (\code{NULL} instead of
    #'   the list is possible for all the default parameters). \cr
    #'   Following parameters are allowed for the \code{bggGames} class with
    #'   default values in parentheses:
    #'   \itemize{
    #'       \item{\code{pretty_names}}{ - (\code{FALSE}) a boolean value,
    #'       should the object should use pretty names,}
    #'       \item{\code{type}}{ - (\code{NULL}) a single string, type of things
    #'       to look for. Possible values: \code{'rpgitem'}, \code{'videogame'},
    #'       \code{'boardgame'}, \code{'boardgameaccessory'},
    #'       \code{'boardgameexpansion'}. \code{NULL} uses all possible values.}
    #'       \item{\code{exact}}{ - (\code{FALSE}) a boolean value, should the
    #'       results be restricted to items that match the \code{query} exactly.}
    #'   }
    initialize = function(query, params = NULL)
    {
        # Assertions -----------------------------------------------------------
        assert_that(.are_strings(query))
        params <- .process_params(params, class = "bggSearch")

        # Connecting to API ----------------------------------------------------
        query_str <- gsub("[[:space:]]", "%20", query)
        query_str <- paste0(query_str, collapse = "%20")

        api_url <- paste0(.bgg_url("api"), "search?query=", query_str)
        api_url <- .extend_url_by_params(api_url, params, class = "bggSearch")

        xml <- read_html(api_url)
        xml <- .xml_expand(xml)


        # Preparing data -------------------------------------------------------
        ids <- as.numeric(sapply(xml, xml_attr, attr = "id"))
        uniq <- !duplicated(ids)
        # uniq <- rep(TRUE, length(ids))
        data <- data.table(objectid = ids[uniq])
        setkey(data, objectid)


        # Setting private variables --------------------------------------------
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
)
