#' Class parameters preprocessing
#'
#' Process, validate and assign default values to parameters of a given class.
#'
#' @param params NULL or a list with params.
#' @param class a single string, class name.
#'
#' @return List with checked ans assigned parameters.
#' @keywords internal
#'
.process_params <- function(params, class)
{
    # Assertions
    assert_that(is.null(params) || is.list(params))
    assert_that(.is_string(class))

    specs <- param_specs[[class]]

    # Unused
    additional <- setdiff(names(params), names(specs))
    if (length(additional) > 0) {
        params <- params[names(params) %in% names(specs)]
        warning("Following parameters are not used for '", class,
                "' class and will be omitted:\n",
                .to_string(additional), call. = FALSE)
    }

    # Using default values
    missing <- setdiff(names(specs), names(params))
    to_add <- lapply(specs[missing], function(x) x$default)

    # Joining with params
    params <- c(to_add, params)
    params <- params[match(names(params), names(specs))]

    # Error function
    .stop_on_error <- function(input) {
        if (isTRUE(input)) {
            return(TRUE)
        }

        # Otherwise
        msg <- gsub("params[[p]]", p, input, fixed = TRUE)
        stop("parameter ", msg, call. = FALSE)
    }

    # Validate
    for (p in names(params)) {
        if (is.null(params[[p]])) next

        valid_str <- specs[[p]]$validator
        validator <- match.fun(valid_str)

        if (valid_str == ".are_strings" && !is.null(specs[[p]]$allowed)) {
            test <- validate_that(
                validator(params[[p]],
                          allowed = specs[[p]]$allowed))
            .stop_on_error(test)

        } else if (valid_str == ".is_positive_integer") {
            test <- validate_that(
                validator(params[[p]],
                          lb = specs[[p]]$min,
                          ub = specs[[p]]$max))
            .stop_on_error(test)

        } else {
            test <- validate_that(validator(params[[p]]))
            .stop_on_error(test)
        }
    }

    return(params)
}


#' Extend given URL by class params
#'
#' This takes an \code{url} and extends it by the given parameters for a
#' certain class
#'
#' @param url a single string.
#' @param params NULL or a list with params.
#' @param class a single string
#'
#' @return A single string with extended URL.
#' @keywords internal
#'
.extend_url_by_params <- function(url, params, class)
{
    # Assertions
    assert_that(.is_string(url))
    assert_that(is.null(params) || is.list(params))
    assert_that(.is_string(class))

    specs <- param_specs[[class]]

    # Preparing params
    for (p in names(params)) {
        param <- params[[p]]

        if (is.null(param)) {
            params[[p]] <- NULL
            next
        }

        intro <- paste0("&", p, "=")

        params[[p]] <- switch(
            EXPR = specs[[p]]$url_type,
            null = NULL,
            flag = {
                if (!param) {
                    result <- NULL
                } else {
                    result <- paste0(intro, "1")
                }

                result
            },
            collapse = paste0(intro, paste0(param, collapse = ",")),
            value = paste0(intro, as.numeric(param))
        )
    }

    # Adding to url
    url <- paste0(url, paste0(unlist(params), collapse = ""))
    return (url)
}
