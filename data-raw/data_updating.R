devtools::load_all()

var_specs <- fread("data-raw/variable_specification.csv")
setkey(var_specs, Variable)

usethis::use_data(var_specs, internal = TRUE, overwrite = TRUE)
rm(var_specs)

