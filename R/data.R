#' All variables that are available for fetching through bggAPI objects
#'
#' Contains names and specification of variables that can be used in
#' `fetch` and `expand` methods of classes that inherit from
#' `bggAPI`.
#'
#' A variable can be used by the object's `extend` method if it's
#' `Scalar` value is `TRUE` or `Compression` is not equal to
#' `"NULL"`.
#'
#' @format A data.table with the following columns:
#' \describe{
#'     \item{`Class`}{a character vector, names of class that is able to
#'     fetch given variable.}
#'     \item{`PrettyName, ClassicName`}{a character vector, names of
#'     variables in the two available styles, `"pretty"` and
#'     `"classic"`.}
#'     \item{`Scalar`}{a logical vector, whether the variable is scalar,
#'     i.e. does the length of a fetched variable vector is equal to the length
#'     of object's `ids` field. Every scalar variable may be used in
#'     the `expand` methods.}
#'     \item{`Stats`}{a logical vector, whether the object needs the
#'     parameter `stats = TRUE` to fetch this variable.}
#'     \item{`Compression`}{a character vector, names of functions that are
#'     used to compress variables to scalar variables when using `fetch(.,
#'     compress = TRUE)` or `expand` for non-scalar variables.
#'     `"NULL"` means that a variable is non-compressible.}
#' }
"bgg_variables"
