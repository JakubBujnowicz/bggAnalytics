# Definition ###################################################################
bggAPI <- R6::R6Class(
    classname = "bggAPI",
    private = list(
        .xml = NULL,
        .api_url = NULL,
        .data = NULL,
        .params = NULL
    ),
    active = list(
        xml = private_getter("xml"),
        api_url = private_getter("api_url"),
        data = private_getter("data")
    ),
    public = list(
        fetch = fetch_external,
        expand = expand_by
    )
)
