.zero2NA <- function(x) fifelse(x == 0, NA_real_, x)

.rmv_fam_sffx <- function(x) str_remove(string = x,
                                        pattern = " Game Rank$| Rank$")
