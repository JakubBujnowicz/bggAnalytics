devtools::load_all()

# Class parameters -------------------------------------------------------------
param_specs <- list()

param_specs$bggGames <- list(
    pretty_names = list(
        validator = "flag",
        default = FALSE,
        url_type = "null"),
    stats = list(
        validator = "flag",
        default = TRUE,
        url_type = "flag")
)
param_specs$bggSearch <- list(
    pretty_names = list(
        validator = "flag",
        default = FALSE,
        url_type = "null"),
    type = list(
        validator = "subset",
        default = NULL,
        url_type = "collapse",
        allowed = c("rpgitem", "videogame", "boardgame",
                    "boardgameaccessory", "boardgameexpansion")),
    exact = list(
        validator = "flag",
        default = FALSE,
        url_type = "flag"
    )
)
param_specs$bggCollection <- list(
    pretty_names = list(
        validator = "flag",
        default = FALSE,
        url_type = "null"),
    stats = list(
        validator = "flag",
        default = TRUE,
        url_type = "flag"),
    brief = list(
        validator = "flag",
        default = FALSE,
        url_type = "flag"),
    own = list(
        validator = "flag",
        default = NULL,
        url_type = "value"),
    rated = list(
        validator = "flag",
        default = NULL,
        url_type = "value"),
    played = list(
        validator = "flag",
        default = NULL,
        url_type = "value"),
    comment = list(
        validator = "flag",
        default = NULL,
        url_type = "value"),
    trade = list(
        validator = "flag",
        default = NULL,
        url_type = "value"),
    want = list(
        validator = "flag",
        default = NULL,
        url_type = "value"),
    wishlist = list(
        validator = "flag",
        default = NULL,
        url_type = "value"),
    wishlistpriority = list(
        validator = "count",
        default = NULL,
        url_type = "value",
        min = 1,
        max = 5),
    minrating = list(
        validator = "count",
        default = NULL,
        url_type = "value",
        min = 1,
        max = 10),
    rating = list(
        validator = "count",
        default = NULL,
        url_type = "value",
        min = 1,
        max = 10)
)

# Uploading --------------------------------------------------------------------
var_specs <- fread("data-raw/variable_specification.csv")
bgg_variables <- var_specs[, c("Class", "PrettyName", "Variable",
                               "Scalar", "Stats", "Compression")]
setnames(bgg_variables, old = "Variable", new = "ClassicName")

usethis::use_data(bgg_variables, overwrite = TRUE)
usethis::use_data(var_specs, param_specs,
                  internal = TRUE, overwrite = TRUE)
rm(var_specs, param_specs, bgg_variables)

devtools::load_all()


