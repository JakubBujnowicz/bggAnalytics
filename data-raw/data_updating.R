devtools::load_all()

var_specs <- fread("data-raw/variable_specification.csv")
setkey(var_specs, Variable)

param_specs <- fread("data-raw/class_params.csv")
setkey(param_specs, Param)

usethis::use_data(var_specs, internal = TRUE, overwrite = TRUE)
usethis::use_data(param_specs, internal = TRUE, overwrite = TRUE)
rm(var_specs)
rm(param_specs)

devtools::load_all()
