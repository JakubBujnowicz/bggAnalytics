#' H-Index
#'
#' Calculate H-Index from the number of game plays. H-Index measures the
#' experience of a player based on reported play counts. It rises only when one
#' plays many different games multiple times. It tries to distinguish players
#' who play a few games really often and these who try every game once and leave
#' it on the shelf from those who have the broad collection of high-count plays.
#'
#' @param num_plays a numeric vector of non-negative integers.
#'
#' @return A single non-negative integer.
#' @export
#'
#' @references
#' \href{https://en.wikipedia.org/wiki/H-index}{H-Index in Wikipedia}
#' \href{https://boardgamegeek.com/thread/953084/whats-your-h-index}{BoardGameGeek
#' thread about H-Index}
#'
#'
#' @examples
#' h_index(0)
#' h_index(c(0, 0, 1, 2, 1))
#' h_index(c(2, 2, 5, 100))
#' h_index(c(2, 3, 5, 100))
#'
h_index <- function(num_plays)
{
    # Assertions
    assert_integerish(num_plays, lower = 0,
                      any.missing = FALSE,
                      min.len = 1)

    x <- num_plays[order(num_plays, decreasing = TRUE)]
    vec <- seq_along(x)

    result <- sum(x >= vec)
    return(result)
}
