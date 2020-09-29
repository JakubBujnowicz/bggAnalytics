load_all()

cl <- bggCollection$new("Beo_", params = list(pretty_names = FALSE))
gm <- bggGames$new(ids = cl$ids, params = list(pretty_names = FALSE))
sr <- bggSearch$new(query = "Terraforming Mars")

cl_fetch <- cl$fetch()
gm_fetch <- gm$fetch()

n_games <- length(cl$ids)

# All games included
length(gm$ids) == n_games

# Any empty
Filter(function(x) length(x) != n_games, cl_fetch)
Filter(function(x) length(x) != n_games, gm_fetch)

# Expansion
cl$expand()
gm$expand()
sr$expand()

cdata <- copy(cl$data)
gdata <- copy(gm$data)
sdata <- copy(sr$data)

# Common cols
cols <- intersect(names(cdata), names(gdata))
all.equal(cdata[, ..cols], gdata[, ..cols])

gm$fetch("designers", compress = TRUE)

# Full data
fdata <- bgg_merge(cdata, gdata)
