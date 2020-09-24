devtools::load_all()

# Class parameters -------------------------------------------------------------
games_specs <- list(
    stats = list(
        validator = ".is_boolean",
        default = TRUE,
        url_type = "flag")
)
search_specs <- list(
    type = list(
        validator = ".are_strings",
        default = NULL,
        url_type = "collapse"),
    exact = list(
        validator = ".is_boolean",
        default = FALSE,
        url_type = "flag"
    )
)

param_specs <- list(bggGames = games_specs,
                    bggSearch = search_specs)

# Uploading --------------------------------------------------------------------
var_specs <- fread("data-raw/variable_specification.csv")
setkey(var_specs, Variable)

# param_specs <- fread("data-raw/class_params.csv")
# setkey(param_specs, Param)

usethis::use_data(var_specs, internal = TRUE, overwrite = TRUE)
usethis::use_data(param_specs, internal = TRUE, overwrite = TRUE)
rm(var_specs, games_specs, param_specs, search_specs)

devtools::load_all()


