load_all()

cl <- bggCollection$new("Beo_")
gm <- bggGames$new(ids = cl$ids)
sr <- bggSearch$new(query = "Terraforming Mars")

cl_fetch <- cl$fetch()
gm_fetch <- gm$fetch()

n_games <- length(cl$ids)

# All games included
length(gm$ids) == n_games

# Any empty
sapply(cl_fetch, length) == n_games
sapply(gm_fetch, length) == n_games

# Expansion
cl$expand()
gm$expand()
