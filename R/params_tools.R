#' Class parameters preprocessing
#'
#' Process, validate and assign default values to parameters of a given class.
#'
#' @param params NULL or a list with params.
#' @param class a single string, class name.
#'
#' @return
#' @keywords internal
#'
.process_params <- function(params, class) {
    # Assertions
    assert_that(is.null(params) || is.list(params))
    assert_that(.is_string(class))

    specs <- param_specs[Class == class]

    # Unused
    additional <- setdiff(names(params), specs$Param)
    if (length(additional) > 0) {
        params <- params[names(params) %in% specs$Param]
        warning("Following parameters are not used for '", class,
                "' class and will be omitted:\n",
                .to_string(additional), call. = FALSE)
    }

    # Using default values
    missing <- setdiff(specs$Param, names(params))
    to_add <- as.list(specs[Param %in% missing, Default])
    names(to_add) <- missing

    # Fixing classes
    .cast2class <- function(x, fun_name) match.fun(paste0("as.", fun_name))(x)
    to_add <- mapply(.cast2class,
                     x = to_add,
                     fun_name = specs[Param %in% missing, Type],
                     SIMPLIFY = FALSE)

    # Joining with params
    params <- c(to_add, params)

    # Validate
    for (p in names(params)) {
        validator <- match.fun( specs[.(p), Validator])
        tryCatch(assert_that(validator(params[[p]])),
                 error = function(e)
                 {
                     msg <- gsub("params[[p]]", p, e, fixed = TRUE)
                     msg <- gsub("Error: ", "", msg)
                     stop(msg, call. = FALSE)
                 }
        )
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
#' @return
#' @keywords internal
#'
.extend_url_by_params <- function(url, params, class) {
    # Assertions
    assert_that(.is_string(url))
    assert_that(is.null(params) || is.list(params))
    assert_that(.is_string(class))

    specs <- param_specs[Class == class]

    # Preparing params
    for (p in names(params)) {
        if (is.null(params[[p]])) next

        params[[p]] <- switch(
            EXPR = specs[.(p), Action],
            add_flag = paste0("&", p, "=", as.numeric(params[[p]])))
    }

    # Adding to url
    url <- paste0(url, paste0(unlist(params), collapse = ""))
    return (url)
}
