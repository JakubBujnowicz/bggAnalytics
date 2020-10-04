# Definition ###################################################################
#' Main class for BGG XML API2
#'
#' @name bggApi
#'
#' @include class_utils.R
#'
bggAPI <- R6::R6Class(
    classname = "bggAPI",
    private = list(
        # Slots
        .data = NULL,
        .xml = NULL,
        .api_url = NULL,
        .params = NULL,
        .timestamp = NULL,

        # Methods
        .get_varspecs = .get_varspecs
    ),
    active = list(
        xml = .private_getter("xml"),
        api_url = .private_getter("api_url"),
        params = .private_getter("params"),
        timestamp = .private_getter("timestamp"),
        data = function(value)
        {
            # Manual so that a copy is returned
            if (missing(value)) {
                return(copy(private$.data))
            } else {
                stop("'data' is a private field and cannot be manually overwritten",
                     call. = FALSE)
            }
        }
    ),
    public = list(
        fetch = .fetch_external,
        expand = .expand_by,
        switch_namestyle = .switch_names
    )
)
