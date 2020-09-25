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
.private_getter <- function(slotname) {
    # Assertion
    assert_that(.is_string(slotname))

    # Closure
    result <- function(value) {
        if (missing(value)) {
            return(private[[paste0(".", slotname)]])
        } else {
            stop(slotname, " is a private field and cannot be manually overwritten",
                 call. = FALSE)
        }
    }

    result <- unenclose(result)
    return(result)
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
#' @return List of variables. Variables marked as 'scalar' in param specification
#'         will be unlisted.
#' @keywords internal
#'
.fetch_internal <- function(xml, variable_names, var_specs) {
    assert_that(.is_nodeset(xml))
    assert_that(.are_strings(variable_names))
    assert_that(is.data.table(var_specs))

    result <- list()
    for (var in variable_names) {
        var_data <- var_specs[.(var)]

        node <- var_data$Node
        attr <- var_data$Attribute
        type <- var_data$Type
        scalar <- var_data$Scalar

        if (node == "NULL") {
            next
        }

        if (attr != "") {
            fun <- match.fun(paste0(".attr2", type))
            fetched <- fun(xml, node, attr)
        } else {
            fun <- match.fun(paste0(".nodes2", type))
            fetched <- fun(xml, node)
        }

        if (scalar) {
            emptys <- sapply(fetched, length) == 0
            fetched[emptys] <- NA
            fetched <- unlist(fetched)
        }

        result[[var_data$Variable]] <- fetched
    }

    names(result) <- variable_names[var_specs$Node != "NULL"]
    return(result)
}

#' @describeIn fetch_internal This should be put in public slot of a class.
#'
.fetch_external <- function(variable_names = NULL) {
    # Internal data
    var_specs <- var_specs[Class == class(self)[1]]

    if (!is.null(variable_names)) {
        assert_that(.are_strings(variable_names))
    } else {
        variable_names <- var_specs$Variable
    }

    if (!all(variable_names %in% var_specs$Variable)) {
        unavailable <- setdiff(variable_names, var_specs$Variable)
        unavailable <- paste0(unavailable, collapse = ", ")
        stop("following variables are not available for bggCollection objects:\n",
             unavailable)
    }

    if (!private$.params$stats) {
        stats_vars <- var_specs[Stats == TRUE, Variable]
        unavailable <- intersect(stats_vars, variable_names)
        if (length(unavailable) > 0) {
            unavailable <- paste0(unavailable, collapse = ", ")
            stop("following variables are not available without stats module:\n",
                 unavailable,
                 ",\nconsider setting 'stats = TRUE' when creating an object")
        }
    }

    result <- .fetch_internal(private$.xml, variable_names, var_specs)
    return(result)
}


# Expanding ####################################################################
#' Generalised expand for every class
#'
#' It is a universal tool for fetching variables from XMLs and adding them
#' to \code{data} slot data.table.
#'
#' @param variable_names Character vector of variable names.
#' @param params List of parameters.
#'
#' @return Nothing, used for side effect.
#' @keywords internal
#'
.expand_by <- function(variable_names = NULL, params = NULL) {
    if (!is.null(variable_names)) {
        assert_that(.are_strings(variable_names))
    }

    fetched <- self$fetch(variable_names)
    var_names <- names(fetched)
    for (i in seq_along(fetched)) {
        set(private$.data, j = var_names[i], value = fetched[[i]])
    }
}
