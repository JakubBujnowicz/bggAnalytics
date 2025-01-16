#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @import checkmate
#' @import data.table
#' @importFrom pryr unenclose
#' @import R6
#' @import stringr
#' @importFrom utils globalVariables
#' @import xml2
## usethis namespace: end
NULL

# To remove notes from CMD Check
globalVariables(
    # Due to R6
    c("self", "private"))


# Options ----------------------------------------------------------------------
.onAttach <- function(libname, packagename) {
    # Amount of elements to print
    options(`.bggAnalytics.print` = 10)
    # Should print messages from functions?
    options(`.bggAnalytics.verbose` = TRUE)
}
