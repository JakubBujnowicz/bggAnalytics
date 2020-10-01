#' @keywords internal
#'
#' @import assertthat
#' @import data.table
#' @importFrom magrittr `%>%`
#' @importFrom pryr unenclose
#' @import R6
#' @import rvest
#' @import stringr
#' @importFrom utils globalVariables
#' @import xml2
#'
NULL

# To remove notes from CMD Check
globalVariables(
    # Due to R6
    c("self", "private",
      # Due to data.table and var_specs
      ".", "Class", "Stats", "Variable", "Node",
      "Compression", "Scalar", "PrettyName"))

# Options ######################################################################
# Amount of elements to print
options(`.bggAnalytics.print` = 10)
# Should print messages from functions?
options(`.bggAnalytics.verbose` = TRUE)

# For testing
options(`.bggAnalytics.username` = "Beo_")
