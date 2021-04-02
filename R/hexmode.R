#' Hexadecimal display
#' 
#' Create and represent integers in hexadecimal format. This extends
#' `base::hexmode`'s functionality by defaulting to coercion to type `integer`
#' if no explicit S3 method for a class has been defined. Moreover, behavior is
#' different with regards to `NA` values in unsuccessful coercion of vectors of
#' types `double` and `character`.
#' @param length A non-negative integer specifying the desire length. Double
#'   values will be coerced to integer: supplying an argument of length other
#'   than one is an error.
#' @export
hexmode <- function(length = 0) {
  structure(vector("integer", length), class = "hexmode")
}

#' Coerce to hexmode
#' 
#' Coerce an object to `hexmode` if possible.
#' @param x An R object.
#' @export
as.hexmode <- function(x) {
  UseMethod("as.hexmode")
}

#' @export
as.hexmode.integer <- function(x) {
  structure(x, class = "hexmode")
}

#' @export
as.hexmode.double <- function(x) {
  x[x != as.integer(x)] <- NA
  structure(as.integer(x), class = "hexmode")
}

#' @export
as.hexmode.character <- function(x) {
  structure(strtoi(x, 16L), class = "hexmode")
}

#' @export
as.hexmode.default <- function(x) {
  structure(as.integer(x), class = "hexmode")
}

#' Test for hexmode class
#' 
#' Check if an object is `hexmode`.
#' @param x An R object.
#' @export
is.hexmode <- function(x) {
  inherits(x, "hexmode")
}
