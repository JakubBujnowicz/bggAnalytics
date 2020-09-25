load_all()

cl <- bggCollection$new("Beo_")
gm <- bggGames$new(ids = 2e4)
sr <- bggSearch$new(query = "Terraforming Mars",
                    params = list(type = "boardgame"))

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
