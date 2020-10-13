# Definition ###################################################################
#' API for Games and Things
#'
#' @description This class provides an interface for games, expansions,
#'   accessories and other things listed on BoardGameGeek. The official
#'   documentation describes `things` as every physical, tangible item.See
#'   \code{\link{bggAPI}} for more details on inherited slots and methods.
#'
#' @details Although this class is named `bggGames`, it inherits it's
#'   functionality from the BoardGameGeek XML API2 `Things` (see References).
#'   The name is motivated by the fact that the whole BoardGameGeek's site as
#'   well as this package is mainly focused on board games.
#'
#' @references
#'   \href{https://boardgamegeek.com/wiki/page/BGG_XML_API2}{BoardGameGeek XML
#'   API2}
#'
#' @export
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
    #' @param chunk_size a positive integer, the maximum length of a chunk that
    #'   \code{ids} are split into. All chunks connect to BoardGameGeek's API
    #'   separately, so lowering this number increases computation time. On the
    #'   other hand if a chunk is too long, URL might be too long to fetch.
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
    initialize = function(ids, chunk_size = 500, params = NULL)
    {
        # Assertions -----------------------------------------------------------
        assert_that(.are_positive_integers(ids))
        params <- .process_params(params, class = "bggGames")

        ids <- unique(ids)

        # Split into chunks
        chunks <- (seq_along(ids) - 1) %/% chunk_size
        chunks <- split(ids, chunks)

        # Getting the API URL
        .get_base_gameurl <- function(x)
        {
            paste0(.bgg_url("api"), "thing?id=", paste0(x, collapse = ","))
        }
        api_url <- sapply(chunks, .get_base_gameurl)
        url_extension <- .extend_url_by_params("", params = params,
                                               class = "bggGames")
        api_url <- paste0(api_url, url_extension)

        # Fetch XMLs
        xml <- lapply(api_url, function(x) .xml_expand(read_xml(x)))
        xml <- .xml_concatenate(xml)

        # Testing IDs
        xml_ids <- as.numeric(xml_attr(xml, attr = "id"))

        # Check for any success
        if (length(intersect(ids, xml_ids)) == 0) {
            stop("None of the given 'ids' were available through BGG API",
                 call. = FALSE)
        }

        # Check for missing
        missing <- setdiff(ids, xml_ids)
        if (length(missing) > 0) {
            warning("Following ids were not available through BGG API:\n",
                    squeeze(missing), call. = FALSE)
        }

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
