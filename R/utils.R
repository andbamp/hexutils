#' @useDynLib hexutils, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

#' Matching a sequence of bytes
#' 
#' Locates a sequence of binary data inside a vector of binary data.
#' @param x Vector of type `raw` where matches are sought.
#' @param block A sequence of bytes to be matched in the given `raw` vector.
#'   Coerced by `as.raw` to a `raw` vector if possible. Byte-sized `integer` and
#'   `hexmode` vectors are valid.
#' @param cpp Setting to `TRUE` makes use of the C++ implementation.
#' @return Vector with the positions of the matches.
#' @export
hexfind <- function(x, block, cpp = TRUE) {
  block <- as.raw(block)
  if(cpp) {
    return(hexfind_cpp(x, block))
  }
  if(length(block) > length(x)) {
    return(integer(0));
  }
  index <- seq(length(x) - length(block) + 1)
  for(i in seq_along(block)) {
    index <- index[x[index + i - 1L] == block[i]]
  }
  index
}

#' Split the elements of a raw vector
#' 
#' Split the elements of a raw vector into chunks according to the matches
#' to byte sequence `split` within them.
#' 
#' @param x Vector of type `raw`.
#' @param split Sequence of bytes to use for splitting. Coerced by `as.raw` to a
#'   `raw` vector if possible. Byte-sized `integer` and `hexmode` vectors are
#'   valid.
#' @return A list of the same length as the number of splits of the byte
#'   sequence.
#' @export
hexsplit <- function(x, split) {
  split_ind <- hexfind(x, split)
  end <- c(split_ind + length(split) - 1, length(x))
  end <- unique(end)
  start <- c(1, split_ind + length(split))[seq(end)]
  lapply(seq(start), function(i) {
    x[start[i]:end[i]]
  })
}

#' Decode binary data
#' 
#' Decode a block of binary data based on a specified character encoding.
#' 
#' @param block Vector of type `raw`.
#' @param encoding `data.frame` or `list` of length 2. First item denotes byte,
#'   second item denotes equivalent decoded character.
#' @param nomatch Character on to which bytes not on the character map should
#'   be decoded.
#' @param collapse An optional character string to separate the results, as in
#'   paste.
#' @return A list of the same length as the number of splits of the byte
#'   sequence.
#' @export
hexdecode <- function(block, encoding, nomatch = NA, collapse = NULL) {
  names(encoding) <- c("hex", "char")
  hex <- encoding[["hex"]]
  if(class(hex) != "raw") {
    hex <- as.raw(as.hexmode(hex))
  }
  char <- encoding[["char"]]
  decoded <- char[match(block, hex)]
  decoded[is.na(decoded)] <- nomatch
  if(is.null(collapse)) {
    return(decoded)
  }
  paste(decoded, collapse = collapse)
}
