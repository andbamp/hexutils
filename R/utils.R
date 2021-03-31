#' Convert hexadecimal string to integer
#' 
#' Utility function for converting hexadecimal string either with prefix `0x` 
#' (eg. `0x5b`) or without (eg. `5b`) into integers. If integers are given as
#' input, they are returned.
#' 
#' @param x Character vector of hexadecimal strings (prefix `0x` optional).
#' @return Numeric vector of corresponding decimal integers.
#' @export
hextoi <- function(x) {
  if(is.numeric(x)) {
    x <- ifelse(x %% 1 == 0, x, NA)
    return(x)
  }
  x <- ifelse(is.character(x) & !grepl("^0x", x), paste0("0x", x), x)
  strtoi(x, 16L)
}

#' Split the elements of a raw vector
#' 
#' Split the elements of a raw vector into chunks according to the matches
#' to byte sequence `split` within them.
#' 
#' @param block Vector of type `raw`.
#' @param split Sequence of bytes represented as a character string, eg.
#'   `"\\x50"` or `"\\x50\\x51"`.
#' @return A list of the same length as the number of splits of the byte
#'   sequence.
#' @export
hexsplit <- function(block, split) {
  split <- charToRaw(split)
  end <- seq_along(block)
  for(i in seq_along(split)) {
    end <- end[block[end + i - 1L] == split[i]]
  }
  end <- c(end + length(split) - 1, length(block))
  end <- unique(end)
  begin <- c(1, end[1:length(end) - 1] + 1)
  lapply(seq(begin), function(i) {
    block[begin[i]:end[i]]
  })
}
