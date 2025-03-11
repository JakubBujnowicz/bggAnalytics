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
.onLoad <- function(libname, pkgname)
{
    .set_default_options(
        # Amount of elements to print
        bggAnalytics.print = 10,
        # Should print messages from functions?
        bggAnalytics.verbose = TRUE)
}

