library(magrittr)
library(hexView)

read_hex_between <- function(path, from, to, sep = NULL) {
  from <- strtoi(from, 16L)
  to <- strtoi(to, 16L)
  nbytes <- to - from
  block <- readRaw(
    file = path,
    width = nbytes,
    offset = from,
    nbytes = nbytes
  ) %>%
    as.character(
      showOffset = FALSE,
      showHuman = FALSE
    )
  if(!is.null(sep)) {
    block %<>% 
      strsplit(split = sep) %>% 
      unlist %>% 
      trimws
  }
  block
}

read_hex_block <- function(path, offset = 0, width = 16, length = 1) {
  offset <- strtoi(offset, 16L)
  nbytes <- width * length
  readRaw(
    file = path,
    width = width,
    offset = offset,
    nbytes = nbytes
  ) %>%
    as.character(
      showOffset = FALSE,
      showHuman = FALSE
    )
}
