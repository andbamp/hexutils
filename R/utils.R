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
    hex <- as.raw(hextoi(hex))
  }
  char <- encoding[["char"]]
  decoded <- char[match(block, hex)]
  decoded[is.na(decoded)] <- nomatch
  if(is.null(collapse)) {
    return(decoded)
  }
  paste(decoded, collapse = collapse)
}
