# Documentation ################################################################
#' @title Generalised Distribution Object
#'
#' @description A generalised distribution object for defining custom probability distributions
#'   as well as serving as the parent class to specific, familiar distributions. Common
#'   mathematical and statistical methods for distributions are defined here with approximate numerical
#'   calculations (as opposed to analytical results).
#'
#' @name bggAPI
#'
#' @keywords internal
#'
#' @include class_utils.R
#'
NULL

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
