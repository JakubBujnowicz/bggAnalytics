# Documentation ################################################################
#' @title Generalised Distribution Object
#'
#' @description A generalised distribution object for defining custom probability distributions
#'   as well as serving as the parent class to specific, familiar distributions. Common
#'   mathematical and statistical methods for distributions are defined here with approximate numerical
#'   calculations (as opposed to analytical results).
#'
#' @name bggGames
#'
#' @export
#'
NULL

# Definition ###################################################################
bggGames <- R6Class(
    classname = "bggGames",
    inherit = bggAPI,
    private = list(
        .ids = numeric()
    ),
    public = list(
        initialize = function(ids) {
            # Assertion
            assert_that(is.numeric(ids), length(ids) != 0,
                        noNA(ids), all(ids %% 1 == 0), all(ids > 0))

            ids <- unique(ids)

            api_url <- bgg_url("api")
            api_url <- paste0(api_url, "thing?id=", paste0(ids, collapse = ","))

            xml <- read_html(api_url)
            xml <- expand_xml(xml)

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
            private$.data <- data.table(objectid = ids)
        },
        print = function(n_show = 5) {
            string <- paste0(
                "---- bggGames ----",
                "\nGames data API.\n",
                "\n* IDs: ", compress(private$.ids,
                                      n_show = n_show),
                "\n* Variables: ", compress(names(private$.data),
                                            n_show = n_show))

            cat(string)
        }
    ),
    active = list(
        ids = private_getter("ids")
    )
)
