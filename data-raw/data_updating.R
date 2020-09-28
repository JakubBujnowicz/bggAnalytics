devtools::load_all()

# Class parameters -------------------------------------------------------------
param_specs <- list()

param_specs$bggGames <- list(
    pretty_names = list(
        validator = ".is_boolean",
        default = FALSE,
        url_type = "null"),
    stats = list(
        validator = ".is_boolean",
        default = TRUE,
        url_type = "flag")
)
param_specs$bggSearch <- list(
    pretty_names = list(
        validator = ".is_boolean",
        default = FALSE,
        url_type = "null"),
    type = list(
        validator = ".are_strings",
        default = NULL,
        url_type = "collapse",
        allowed = c("rpgitem", "videogame", "boardgame",
                    "boardgameaccessory", "boardgameexpansion")),
    exact = list(
        validator = ".is_boolean",
        default = FALSE,
        url_type = "flag"
    )
)
param_specs$bggCollection <- list(
    pretty_names = list(
        validator = ".is_boolean",
        default = FALSE,
        url_type = "null"),
    stats = list(
        validator = ".is_boolean",
        default = TRUE,
        url_type = "flag"),
    brief = list(
        validator = ".is_boolean",
        default = FALSE,
        url_type = "flag"),
    own = list(
        validator = ".is_boolean",
        default = NULL,
        url_type = "value"),
    rated = list(
        validator = ".is_boolean",
        default = NULL,
        url_type = "value"),
    played = list(
        validator = ".is_boolean",
        default = NULL,
        url_type = "value"),
    comment = list(
        validator = ".is_boolean",
        default = NULL,
        url_type = "value"),
    trade = list(
        validator = ".is_boolean",
        default = NULL,
        url_type = "value"),
    want = list(
        validator = ".is_boolean",
        default = NULL,
        url_type = "value"),
    wishlist = list(
        validator = ".is_boolean",
        default = NULL,
        url_type = "value"),
    wishlistpriority = list(
        validator = ".is_positive_integer",
        default = NULL,
        url_type = "value",
        min = 1,
        max = 5),
    minrating = list(
        validator = ".is_positive_integer",
        default = NULL,
        url_type = "value",
        min = 1,
        max = 10),
    rating = list(
        validator = ".is_positive_integer",
        default = NULL,
        url_type = "value",
        min = 1,
        max = 10)
)

# Uploading --------------------------------------------------------------------
var_specs <- fread("data-raw/variable_specification.csv")

# param_specs <- fread("data-raw/class_params.csv")
# setkey(param_specs, Param)

usethis::use_data(var_specs, param_specs,
                  internal = TRUE, overwrite = TRUE)
rm(var_specs, param_specs)

devtools::load_all()


