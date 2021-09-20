#' Squeeze integers into a single string
#'
#' This acts similar to \code{\link[base]{toString}} function, but it tries to
#' make the string as short as possible by squeezing sequences of integers into
#' boundary values only. Please see the examples section. \code{unsqueez}
#' reverses this operation.
#'
#' @param integers a numeric vector if integers.
#'
#' @return \code{squeeze} returns a vector of characters, \code{unsqueeze}
#'   returns a list of numerics or a numeric vector.
#'
#' @author Jakub Bujnowicz \email{bujnowiczgithub@@gmail.com}
#' @export
#'
#' @examples
#' integers <- c(1, 3:5, NA, 27:38, 10:13, 9:11, 6)
#' squeeze(integers)
#'
#' unsqueeze(squeeze(integers))
#' setdiff(na.omit(integers), unsqueeze(squeeze(integers)))
#'
squeeze <- function(integers)
{
    # Assertions
    assert_integerish(integers)

    # Get sorted integer vector
    ints <- integers[!is.na(integers)]
    ints <- unique(ints)

    if (is.unsorted(ints)) {
        ints <- sort(ints)
    }

    # Empty or just a single number
    if (length(ints) == 1) {
        return(as.character(ints))
    } else if (length(ints) == 0) {
        return("")
    }


    dff <- diff(ints)
    seqs <- rle(dff)

    lens <- seqs$lengths
    pos <- cumsum(lens) - lens + 1

    val_1 <- seqs$values == 1
    pos_1 <- pos[val_1]
    len_1 <- lens[val_1]

    starts <- ints[pos_1]
    ends <- starts + len_1
    ranges <- paste(starts, ends, sep = "-")
    missing <- setdiff(ints,
                       unlist(mapply(seq, from = starts, to = ends)))

    result <- c(missing, ranges)
    first_vals <- c(missing, starts)
    result <- result[order(first_vals)]
    result <- toString(result)
    return(result)
}

#' @rdname squeeze
#'
#' @param strings a character vector of strings, preferably outputs of
#'   \code{squeeze}.
#' @param strict a logical value, decides whether the output should be strictly
#'   a list. If \code{FALSE} and the \code{strings} is a single string, the
#'   function returns an atomic vector instead.
#'
#' @export
#'
unsqueeze <- function(strings, strict = FALSE)
{
    # Assertions
    assert_character(strings)
    assert_flag(strict)

    # Functions
    .mat2num <- function(x)
    {
        suppressWarnings(mode(x) <- "numeric")
        return(x)
    }
    .unfold <- function(x, y)
    {
        if (is.na(y)) {
            return(x)
        } else {
            return(seq(x, y, by = 1))
        }
    }

    res <- str_split(strings, pattern = ", ")
    res <- lapply(res, str_split_fixed, pattern = "-", n = 2)
    res <- lapply(res, .mat2num)
    res <- lapply(res,
                  function(m) unlist(mapply(.unfold,
                                            x = m[, 1],
                                            y = m[, 2])))

    if (!strict && length(res) == 1) {
        res <- unlist(res)
    }

    return(res)
}
