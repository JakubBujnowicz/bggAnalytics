# Definition ###################################################################
#' API for Games and Things
#'
#' @description This class provides an interface for games, expansions,
#'   accesories and other things listed on BGG. The official documentation
#'   describes `things` as every physical, tangible item.See
#'   \code{\link{bggAPI}} for more details on inherited slots and methods.
#'
#' @details Although this class is named `bggGames`, it inherits it's
#'   functionality from the BGG XML API2 `Things` (see References). The name is
#'   motivated by the fact that the whole BGG's site as well as this package is
#'   mainly focused on board games.
#'
#' @references \href{https://boardgamegeek.com/wiki/page/BGG_XML_API2}{BGG XML
#' API2}
#'
#' @include bggAPI.R
#'
bggGames <- R6Class(
    classname = "bggGames",
    inherit = bggAPI,

    public = list(
    # Initialize ---------------------------------------------------------------
    #' @description Object initialization.
    #'
    #' @param ids a numeric vector of positive integers, IDs of games/things to
    #'   include in the object.
    #' @param params a list of object parameters. If not all the parameters are
    #'   included in the list, default values are used (\code{NULL} instead of
    #'   the list is possible for all the default parameters). \cr
    #'   Following parameters are allowed for the \code{bggGames} class with
    #'   default values in parentheses:
    #'   \itemize{
    #'       \item{\code{pretty_names}}{ - (\code{FALSE}) a boolean value,
    #'       should the object should use pretty names,}
    #'       \item{\code{stats}}{ - (\code{TRUE}) a boolean value, should the
    #'       ranking and rating stats be included for every item. Note that some
    #'       variables require that \code{stats} is \code{TRUE}.}
    #'   }
    initialize = function(ids, params = NULL)
    {
        # Assertions -----------------------------------------------------------
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
        xml_ids <- .attr2number(xml, xpath = ".", attr = "id")

        # Check for any success
        ids <- intersect(ids, xml_ids)
        if (length(ids) == 0) {
            stop("None of the given 'ids' were available through BGG API",
                 call. = FALSE)
        }

        # Check for missing
        missing <- setdiff(ids, xml_ids)
        if (length(missing) > 0) {
            warning("Following ids were not available through BGG API:\n",
                    toString(missing), call. = FALSE)
        }

        # Sorting
        # Sorting IDs and XML
        ids_order <- order(xml_ids)
        ids <- xml_ids[ids_order]
        xml <- xml[ids_order]

        # Setting private variables --------------------------------------------
        private$.timestamp <- Sys.time()
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
            "----- bggGames -----",
            "\nGames data API.\n",
            "Creation timestamp: ", private$.timestamp,
            ".\nThe data contains ", nr, " ", .plural("object", nr), " and ",
            nc, " ", .plural("variable", nc), ".\n\n")
        cat(string)
        cat("------- Data -------\n")
        print(private$.data, nrows = n_show, trunc.cols = TRUE)
    })
)
