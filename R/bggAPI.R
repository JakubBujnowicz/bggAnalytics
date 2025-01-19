# Definition ###################################################################
#' Interface for BoardGameGeek's XML API2
#'
#' @description This is a class that works as a main interface to
#'   BoardGameGeek's API. All other `bgg*` classes inherit from
#'   `bggAPI`. Furthermore, there is no initialization method for
#'   `bggAPI` as it serves just as a super class for other classes.
#'
#' @references
#'   [BoardGameGeek XML
#'   API2](https://boardgamegeek.com/wiki/page/BGG_XML_API2)
#'
#' @export
#' @include class_utils.R
#'
bggAPI <- R6::R6Class(
    classname = "bggAPI",
    private = list(
        # Fields
        .ids = NULL,
        .data = NULL,
        .xml = NULL,
        .api_url = NULL,
        .params = NULL,
        .timestamp = NULL,

        # Methods
        .get_varspecs = .get_varspecs
    ),

    active = list(
        #' @field ids Numeric vector of positive integers. Contains IDs of all
        #'   BoardGameGeek items that were requested and are present in
        #'   `data`. The vector is sorted by default.
        ids = .private_getter("ids"),

        #' @field data Data.table with currently fetched variables from the
        #'   object's XML. It has a row count equal to the length of `ids`.
        #'   Note that the copy is returned, so no modification of `data`
        #'   is possible outside of class methods.
        data = function(value)
        {
            # Manual so that a copy is returned
            if (missing(value)) {
                return(copy(private$.data))
            } else {
                stop("'data' is a private field and cannot be manually overwritten",
                     call. = FALSE)
            }
        },

        #' @field xml An XML nodeset obtained through the BoardGameGeek's API.
        #'   It's length is equal to the length of `ids`.
        xml = .private_getter("xml"),

        #' @field api_url A character vector of one or more strings with URLs
        #'   used to fetch XMLs.
        api_url = .private_getter("api_url"),

        #' @field params A list with object parameters.
        params = .private_getter("params"),

        #' @field timestamp The date (with time) of the object creation and,
        #'   what follows, the last moment that the data was certainly up to
        #'   date.
        timestamp = .private_getter("timestamp")
    ),

    public = list(
    # Fetch --------------------------------------------------------------------
    #' @description Fetches variables with given `variable_names` from the
    #'   object's `xml`. Returns them as a list. This is a main method of
    #'   getting non-scalar variables (as they are hard to fit into a
    #'   data.table).
    #'
    #' @param variable_names a character vector with names of variables to
    #'   fetch.
    #' @param compress a logical value, decides whether the fetched variables
    #'   should be compress into a scalar form (if possible).
    fetch = function(variable_names = NULL,
                     compress = FALSE)
    {
        # Assign to avoid NOTEs while checking the package
        Variable <- NULL
        Stats <- NULL

        # Internal data
        obj_class <- class(self)[1]
        specs <- private$.get_varspecs()

        if (!is.null(variable_names)) {
            assert_character(variable_names,
                             any.missing = FALSE,
                             min.len = 1,
                             unique = TRUE)
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

        # Loop for every variable ----------------------------------------------
        xml <- private$.xml

        result <- list()
        for (var in variable_names) {
            vs <- specs[Variable == var]

            # Extract ----------------------------------------------------------
            if (vs$Custom != "") {
                fun <- .internal_fun(paste0(".fetch_", vs$Custom))
                fetched <- fun(xml)
            } else if (vs$Attribute != "") {
                fun <- .internal_fun(paste0(".attr2", vs$Type))
                fetched <- fun(xml = xml,
                               xpath = vs$Node,
                               attr = vs$Attribute,
                               scalar = vs$Scalar)
            } else {
                fun <- .internal_fun(paste0(".nodes2", vs$Type))
                fetched <- fun(xml = xml,
                               xpath = vs$Node,
                               scalar = vs$Scalar)
            }

            # Apply postprocessing function ------------------------------------
            if (vs$Postprocessing != "") {
                post_fun <- .internal_fun(paste0(".", vs$Postprocessing))

                if (vs$Scalar) {
                    fetched <- post_fun(fetched)
                } else {
                    fetched <- lapply(fetched, post_fun)
                }
            }

            # Compression ------------------------------------------------------
            if (compress) {
                if (vs$Compression == "toString") {
                    fetched <- sapply(fetched, toString)
                }
                if (vs$Compression == "squeeze") {
                    fetched <- suppressWarnings(lapply(fetched, as.numeric))
                    fetched <- sapply(fetched, squeeze)
                }
            }

            result[[vs$Variable]] <- fetched
        }

        # Naming
        names(result) <- variable_names
        return(result)
    },


    # Expand ---------------------------------------------------------------
    #' @description Expands the `data` table by given `variable_names`
    #'   by reference. For the list of available variables for every object
    #'   check the \code{\link{bgg_variables}} dataset.
    #'
    #' @param variable_names a character vector with names of variables to add
    #'   to `data`.
    expand = function(variable_names = NULL)
    {
        # Assign to avoid NOTEs while checking the package
        Variable <- NULL
        Scalar <- NULL
        Compression <- NULL

        specs <- private$.get_varspecs()

        if (!is.null(variable_names)) {
            assert_character(variable_names,
                             any.missing = FALSE,
                             min.len = 1,
                             unique = TRUE)

            # Already present and omitted
            existing <- intersect(variable_names, names(private$.data))
            if (length(existing) > 0) {
                variable_names <- setdiff(variable_names, existing)
                msg <- paste0("following variables already exist in the 'data' slot ",
                              "and will be omitted:\n",
                              .to_string(existing))
            }
        } else {
            available <- specs[Scalar == TRUE | Compression != "NULL", Variable]
            variable_names <- setdiff(available, names(private$.data))

            # Which are added automatically
            if (length(variable_names) > 0) {
                msg <- paste0("expanding by all the available variables:\n",
                              .to_string(variable_names))
            }
        }

        # Non-scalar and can't be compressed
        non_scalars <- specs[Scalar == FALSE & Compression == "NULL", Variable]
        nonsc_vars <- intersect(non_scalars, variable_names)
        if (length(nonsc_vars) > 0) {
            stop("following variables are non-scalar and cannot be used within ",
                 "the 'expand' method (use 'fetch' instead):\n",
                 .to_string(nonsc_vars))
        }

        # Expanding
        if (length(variable_names) != 0) {
            fetched <- self$fetch(variable_names, compress = TRUE)
            var_names <- names(fetched)
            for (i in seq_along(fetched)) {
                set(private$.data, j = var_names[i], value = fetched[[i]])
            }

            if (exists("msg", inherits = FALSE)) {
                message(msg)
            }

            return(invisible(TRUE))
        } else {
            message("there was nothing to add to the 'data' slot")
            return(invisible(FALSE))
        }
    },


    # Switch namestyle -----------------------------------------------------
    #' @description Switches between two styles of naming the variables:
    #'   `'classic'` and `'pretty'`. The former is a default value and
    #'   uses code names concordant with BoardGameGeek's naming convention in
    #'   the XMLs. The latter is more intuitive and uses UpperCamelCase.
    #'
    #' @return Returns `TRUE` or `FALSE` invisibly depending on
    #'   whether the names have been switched.
    #'
    #' @param to a single string, either `'classic'` or `'pretty'`.
    switch_namestyle = function(to)
    {
        # Assertions
        assert_choice(to, choices = c("pretty", "classic"))

        # Assign to avoid NOTEs while checking the package
        Class <- NULL

        specs <- var_specs[Class == class(self)[1]]
        current <- names(private$.data)

        has_pn <- private$.params$pretty_names
        pn_str <- fifelse(has_pn, "pretty", "classic")
        if (pn_str == to) {
            return(invisible(FALSE))
        }

        if (to == "pretty") {
            new <- specs$PrettyName
            old <- specs$Variable

            private$.params$pretty_names <- TRUE
        } else if (to == "classic") {
            new <- specs$Variable
            old <- specs$PrettyName

            private$.params$pretty_names <- FALSE
        }

        replacement <- new[match(current, old)]
        setnames(private$.data, old = current, new = replacement)

        return(invisible(TRUE))
    })
)
