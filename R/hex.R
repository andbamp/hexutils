hextoi <- function(x) {
  if(is.numeric(x)) {
    x <- ifelse(x %% 1 == 0, x, NA)
    return(x)
  }
  x <- ifelse(is.character(x) & !grepl("^0x", x), paste0("0x", x), x)
  strtoi(x, 16L)
}

hex_reader <- function(path) {
  env <- environment()
  
  offset <- 0
  
  reset <- function() {
    assign("offset", 0, envir = env)
  }
  
  set_offset <- function(x) {
    assign("offset", hextoi(x), envir = env)
  }
  
  read_block <- function(size = 16) {
    con <- file(path, "rb")
    seek(con, offset)
    assign("offset", offset + size, envir = env)
    block <- readBin(con, what = "raw", n = size)
    close(con)
    block
  }
  
  read_between <- function(start, end) {
    con <- file(path, "rb")
    seek(con, hextoi(start))
    size <- hextoi(end) - hextoi(start)
    block <- readBin(con, what = "raw", n = size)
    close(con)
    block
  }
  
  parse <- function(block, sep) {
    sep <- charToRaw(sep)
    end <- seq_along(block)
    for(i in seq_along(sep)) {
      end <- end[block[end + i - 1L] == sep[i]]
    }
    end <- c(end + length(sep) - 1, length(block))
    begin <- c(1, end[1:length(end) - 1] + 1)
    lapply(seq(begin), function(i) {
      block[begin[i]:end[i]]
    })
  }
  
  parse_block <- function(size = 16, sep) {
    block <- read_block(size)
    parse(block, sep)
  }
  
  parse_between <- function(start, end, sep) {
    block <- read_between(start, end)
    parse(block, sep)
  }
  
  list(
    offset = function() offset,
    set_offset = set_offset,
    reset = reset,
    read_block = read_block,
    read_between = read_between,
    parse_block = parse_block,
    parse_between = parse_between
  )
}
