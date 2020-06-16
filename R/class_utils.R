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
    assert_that(is.string(slotname))

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

expand_by <- function(data, xml, variable_names, params = NULL) {
    assert_that(is.data.table(data))
    assert_that(is.character(variable_names), noNA(variable_names))

    for (var in variable_names) {
        expand_function <- match.fun(paste0("expand_by_", var))
        expand_function(data = data, xml = xml, params = params)
    }
}

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
fetch_internal <- function(xml, variable_names, var_specs) {
    assert_that(inherits(xml, "xml_nodeset"))
    assert_that(is.character(variable_names), noNA(variable_names))
    assert_that(is.data.table(var_specs))

    result <- list()
    for (var in variable_names) {
        var_data <- var_specs[.(var)]

        node <- var_data$Node
        attr <- var_data$Attribute
        type <- var_data$Type
        scalar <- var_data$Scalar

        if (attr != "") {
            fun <- match.fun(paste0("attr2", type))
            fetched <- fun(xml, node, attr)
        } else {
            fun <- match.fun(paste0("nodes2", type))
            fetched <- fun(xml, node)
        }

        if (scalar) {
            fetched <- unlist(fetched)
        }

        result[[var_data$Variable]] <- fetched
    }
    names(result) <- variable_names
    return(result)
}

#' @describeIn fetch_internal This should be put in public slot of a class.
#'
fetch_external <- function(variable_names = NULL) {
    # Internal data
    var_specs <- var_specs[Class == class(self)[1]]

    if (!is.null(variable_names)) {
        assert_that(is.character(variable_names),
                    noNA(variable_names),
                    not_empty(variable_names))
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

    result <- fetch_internal(private$.xml, variable_names, var_specs)
    return(result)
}
