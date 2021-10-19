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
    assert_list(params, null.ok = TRUE)
    assert_string(class)

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

    # Validate
    for (p in names(params)) {
        if (is.null(params[[p]])) next

        valid_str <- specs[[p]]$validator

        if (valid_str == "subset") {
            assert_subset(params[[p]], choices = specs[[p]]$allowed,
                          .var.name = p)

        } else if (valid_str == "count") {
            assert_integerish(params[[p]],
                              lower = specs[[p]]$min,
                              upper = specs[[p]]$max,
                              len = 1,
                              .var.name = p)
        } else {
            validator <- get(paste0("assert_", valid_str),
                             envir = asNamespace("checkmate"))

            validator(params[[p]], .var.name = p)
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
    assert_string(url)
    assert_list(params, null.ok = TRUE)
    assert_string(class)

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
    return(url)
}
