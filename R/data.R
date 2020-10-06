#' All variables that are available for fetching through bggAPI objects
#'
#' Contains names and specification of variables that can be used in
#' \code{fetch} and \code{expand} methods of classes that inherit from
#' \code{bggAPI}.
#'
#' A variable can be used by the object's \code{extend} method if it's
#' \code{Scalar} value is \code{TRUE} or \code{Compression} is not equal to
#' \code{"NULL"}.
#'
#' @format A data.table with the following columns:
#' \describe{
#'     \item{\code{Class}}{a character vector, names of class that is able to
#'     fetch given variable.}
#'     \item{\code{PrettyName, ClassicName}}{a character vector, names of
#'     variables in the two available styles, \code{"pretty"} and
#'     \code{"classic"}.}
#'     \item{\code{Scalar}}{a logical vector, whether the variable is scalar,
#'     i.e. does the length of a fetched variable vector is equal to the length
#'     of object's \code{ids} field. Every scalar variable may be used in
#'     the \code{expand} methods.}
#'     \item{\code{Stats}}{a logical vector, whether the object needs the
#'     parameter \code{stats = TRUE} to fetch this variable.}
#'     \item{\code{Compression}}{a character vector, names of functions that are
#'     used to compress variables to scalar variables when using \code{fetch(.,
#'     compress = TRUE)} or \code{expand} for non-scalar variables.
#'     \code{"NULL"} means that a variable is non-compressible.}
#' }
"bgg_variables"
