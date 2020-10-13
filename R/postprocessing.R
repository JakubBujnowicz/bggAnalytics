.zero2NA <- function(x) fifelse(x == 0, NA_real_, x)

.remove_ranks <- function(x) str_remove(string = x, pattern = " Rank$")
