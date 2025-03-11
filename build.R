library(devtools)

# 1: Documenting
document()
# roxygenise(clean = TRUE)

# 2: Checking
check(document = FALSE, cran = TRUE)

# 3: Building
build(path = "tars")
# build_manual(path = "manuals")

# 4: Installing
install()

# Update data
source("data-raw/data_updating.R")

# Update version
usethis::use_version()


# Release ----------------------------------------------------------------------
spell_check()
check_rhub()
check_win_devel()

{
    # 5: Release
    release()
}

