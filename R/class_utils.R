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
            stop(slotname, " is a private field and cannot be manually overwritten",
                 call. = FALSE)
        }
    }

    result_fun <- unenclose(result_fun)
    return(result_fun)
}


#' Switch the style of object's data colnames
#'
#' This change between \code{PrettyName} and \code{Variable} names for variables
#' in \code{bggAPI} objects.
#'
#' @param to a single string, style of names to switch to.
#'
#' @return Nothing, used for it's side effect.
#' @keywords internal
#'
.switch_names <- function(to)
{
    assert_that(.is_string(to, allowed = c("pretty", "normal")))

    specs <- var_specs[Class == class(self)[1]]
    current <- names(private$.data)

    if (to == "pretty") {
        new <- specs$PrettyName
        old <- specs$Variable

        private$.params$pretty_names <- TRUE
    } else if (to == "normal") {
        new <- specs$Variable
        old <- specs$PrettyName

        private$.params$pretty_names <- FALSE
    }

    replacement <- new[match(current, old)]
    setnames(private$.data, old = current, new = replacement)
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
#'
#' @name fetches
#'
#' @return List of variables. Variables marked as 'scalar' in param
#'   specification will be unlisted.
#' @keywords internal
#'
NULL

#' @describeIn fetches Internal implementation of variable extraction
#'   (fetching).
#'
.fetch_internal <- function(xml, variable_names, var_specs,
                            compress = FALSE)
{
    # Assertions
    assert_that(.is_nodeset(xml))
    assert_that(.are_strings(variable_names))
    assert_that(is.data.table(var_specs))
    assert_that(.is_boolean(compress))

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

        # Skip - nothing to fetch
        if (node == "NULL") {
            next
        }

        # Extract --------------------------------------------------------------
        if (attr != "") {
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
        }

        result[[specs$Variable]] <- fetched
    }

    # Naming
    res_names <- var_specs[Node != "NULL" & Variable %in% variable_names,
                           Variable]
    names(result) <- res_names

    return(result)
}

#' @describeIn fetches This should be put in public slot of a class.
#'
.fetch_external <- function(variable_names = NULL, ...)
{
    # Internal data
    obj_class <- class(self)[1]
    specs <- private$.get_varspecs()

    if (!is.null(variable_names)) {
        assert_that(.are_strings(variable_names))
    } else {
        variable_names <- specs$Variable
    }

    if (!all(variable_names %in% specs$Variable)) {
        unavailable <- setdiff(variable_names, specs$Variable)
        stop("following variables are not available for '", obj_class,
             "' objects:\n", .to_string(unavailable))
    }

    stats_param <- private$.params$stats
    if (!is.null(stats_param) && !stats_param) {
        stats_vars <- specs[Stats == TRUE, Variable]
        unavailable <- intersect(stats_vars, variable_names)
        if (length(unavailable) > 0) {
            stop("following variables are not available without stats module:\n",
                 .to_string(unavailable),
                 ",\nconsider setting 'stats = TRUE' when creating an object")
        }
    }

    result <- .fetch_internal(xml = private$.xml,
                              variable_names = variable_names,
                              var_specs = specs,
                              ...)
    return(result)
}


# Expanding ####################################################################
#' Generalised expand for every class
#'
#' It is a universal tool for fetching variables from XMLs and adding them to
#' \code{data} slot data.table.
#'
#' @param variable_names Character vector of variable names.
#' @param params List of parameters.
#'
#' @return Nothing, used for side effect.
#' @keywords internal
#'
.expand_by <- function(variable_names = NULL, params = NULL)
{
    if (!is.null(variable_names)) {
        assert_that(.are_strings(variable_names))

        existing <- intersect(variable_names, names(private$.data))
        if (length(existing) > 0) {
            variable_names <- setdiff(variable_names, existing)
            message("following variables already exist in the 'data' slot ",
                    "and will be omitted:\n",
                    .to_string(existing))
        }
    } else {
        specs <- private$.get_varspecs()
        variable_names <- setdiff(specs$Variable, names(private$.data))

        if (length(variable_names) > 0) {
            message("expanding by all the missing variables:\n",
                    .to_string(variable_names))
        }
    }

    if (length(variable_names) != 0) {
        fetched <- self$fetch(variable_names, compress = TRUE)
        var_names <- names(fetched)
        for (i in seq_along(fetched)) {
            set(private$.data, j = var_names[i], value = fetched[[i]])
        }
    } else {
        message("there was nothing to add to the 'data' slot")
    }
}
