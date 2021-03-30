#' Convert hexadecimal string to integer
#' 
#' Utility function for converting hexadecimal string either with prefix `0x` 
#' (eg. `0x5b`) or without (eg. `5b`) into integers. If integers are given as
#' input, they are returned.
#' 
#' @param x A character vector of hexadecimal strings (prefix `0x` optional).
#' @return A numeric vector of corresponding decimal integers.
#' @export
hextoi <- function(x) {
  if(is.numeric(x)) {
    x <- ifelse(x %% 1 == 0, x, NA)
    return(x)
  }
  x <- ifelse(is.character(x) & !grepl("^0x", x), paste0("0x", x), x)
  strtoi(x, 16L)
}
