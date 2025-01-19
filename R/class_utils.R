#' Getter for private slots of R6 objects
#'
#' This function returns another function which is able to extract private
#' slots. It decreases redundant code and makes class definitions easier to
#' read.
#'
#' @param slotname Single string, name of private slot.
#'
#' @return Function that extracts the private `slotname` slot from a R6 object.
#' @keywords internal
#'
.private_getter <- function(slotname)
{
    # Assertion
    assert_string(slotname)

    # Closure
    result_fun <- function(value) {
        if (missing(value)) {
            return(private[[paste0(".", slotname)]])
        } else {
            stop("'", slotname, "' is a private field and cannot be manually ",
                 "overwritten", call. = FALSE)
        }
    }

    result_fun <- unenclose(result_fun)
    return(result_fun)
}


#' Get variable specification table for a given class
#'
#' This can be used within `bggAPI` methods to extract rows for a proper
#' class. This also handles the 'pretty names'.
#'
#' @return The data.table with variable specifications
#' @keywords internal
#'
.get_varspecs <- function()
{
    # Assign to avoid NOTEs while checking the package
    Class <- NULL

    obj_cl <- class(self)[1]

    specs <- var_specs[Class == obj_cl]

    if (private$.params$pretty_names) {
        specs <- specs[, -"Variable"]
        setnames(specs, old = "PrettyName", new = "Variable")
    }

    return(specs)
}
