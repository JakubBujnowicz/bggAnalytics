#' @keywords internal
#'
#' @import assertthat
#' @import data.table
#' @importFrom magrittr `%>%`
#' @importFrom pryr unenclose
#' @import R6
#' @import rvest
#' @importFrom utils globalVariables
#' @import xml2
#'
#'
NULL

# To remove notes from CMD Check
globalVariables(c("self", "private"))

# Options ######################################################################
# Amount of elements to print
options(`.bggAnalytics.print` = 10)
# Maximum tries of collection fetching
options(`.bggAnalytics.max_tries` = 3)

# For testing
options(`.bggAnalytics.username` = "Beo_")
