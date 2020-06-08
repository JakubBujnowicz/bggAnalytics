#' Getter for private slots of R6 objects
#'
#' This function returns another function which is able to extract private slots.
#' It decreases redundant code and makes class definitions easier to read.
#'
#' @param slotname Single string, name of private slot.
#'
#' @return Function that extracts the private `slotname` slot from a R6 object.
#' @keywords internal
#'
private_getter <- function(slotname) {
    # Assertion
    assert_that(is.character(slotname), is.scalar(slotname))

    # Closure
    result <- function(value) {
        if (missing(value)) {
            return(private[[paste0(".", slotname)]])
        } else {
            stop(slotname, " cannot be manually overwritten", call. = FALSE)
        }
    }

    result <- unenclose(result)
    return(result)
}
