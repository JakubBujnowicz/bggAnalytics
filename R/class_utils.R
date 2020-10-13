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
    assert_that(.is_string(slotname))

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
#' This can be used within \code{bggAPI} methods to extract rows for a proper
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


# Fetching #####################################################################
#' Generalised fetch for every class
#'
#' It is a universal tool for fetching variables from XMLs, should be used
#' within fetch methods from every bgg* class. It uses internal variable
#' parameter specification to extract data.
#'
#' @param xml XML nodeset.
#' @param variable_names Character vector of variable names to extract.
#' @param var_specs Data.table with parameter specification.
#' @param compress a logical value, whether variables should be compressed.
#'
#' @name fetches
#'
#' @return List of variables. Variables marked as 'scalar' in param
#'   specification will be unlisted.
#' @keywords internal
#'
.fetch_internal <- function(xml, variable_names, var_specs,
                            compress = FALSE)
{
    # Assertions
    assert_that(.is_nodeset(xml))
    assert_that(.are_strings(variable_names))
    assert_that(is.data.table(var_specs))
    assert_that(.is_boolean(compress))

    # Assignment to avoid NOTEs while checking the package
    Variable <- NULL
    Node <- NULL

    # Loop for every variable --------------------------------------------------
    result <- list()
    for (var in variable_names) {
        specs <- var_specs[Variable == var]

        node <- specs$Node
        attr <- specs$Attribute
        type <- specs$Type
        scalar <- specs$Scalar
        val2na <- specs$ValueToNA
        compression <- specs$Compression
        custom <- specs$Custom

        # Extract --------------------------------------------------------------
        if (custom != "") {
            fun <- match.fun(paste0(".fetch_", custom))
            fetched <- fun(xml)
        } else if (attr != "") {
            fun <- match.fun(paste0(".attr2", type))
            fetched <- fun(xml = xml, xpath = node, attr = attr,
                           scalar = scalar)
        } else {
            fun <- match.fun(paste0(".nodes2", type))
            fetched <- fun(xml = xml, xpath = node, scalar = scalar)
        }

        # Replace some values with NAs -----------------------------------------
        if (!is.na(val2na)) {
            fetched[fetched == val2na] <- NA
        }

        # Compression ----------------------------------------------------------
        if (compress) {
            if (compression == "toString") {
                fetched <- sapply(fetched, toString)
            }
            if (compression == "squeeze") {
                fetched <- suppressWarnings(lapply(fetched, as.numeric))
                fetched <- sapply(fetched, squeeze)
            }
        }

        result[[specs$Variable]] <- fetched
    }

    # Naming
    names(result) <- variable_names
    return(result)
}

