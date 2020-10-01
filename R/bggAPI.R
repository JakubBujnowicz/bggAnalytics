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
        .xml = NULL,
        .api_url = NULL,
        .data = NULL,
        .params = NULL,
        .timestamp = NULL,

        # Methods
        .get_varspecs = .get_varspecs
    ),
    active = list(
        xml = .private_getter("xml"),
        api_url = .private_getter("api_url"),
        data = .private_getter("data"),
        params = .private_getter("params"),
        timestamp = .private_getter("timestamp")
    ),
    public = list(
        fetch = .fetch_external,
        expand = .expand_by,
        switch_namestyle = .switch_names
    )
)
