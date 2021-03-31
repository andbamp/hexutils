#' Binary file reader
#' 
#' `hex_reader()` provides convenience methods for reading and parsing binary
#' data.
#' 
#' @param path File path.
#' @param offset Integer or character string representing a hexadecimal number
#'   to set the initial position of binary file connection.
#' @return Interface to methods as list.
#' @export
hex_reader <- function(path, offset = 0) {
  env <- environment()
  
  offset <- hextoi(offset)
  
  reset <- function() {
    assign("offset", 0, envir = env)
  }
  
  set_offset <- function(x) {
    assign("offset", hextoi(x), envir = env)
  }
  
  read_block <- function(size = 16) {
    con <- file(path, "rb")
    on.exit(close(con))
    seek(con, offset)
    assign("offset", offset + size, envir = env)
    readBin(con, what = "raw", n = size)
  }
  
  read_between <- function(start, end) {
    con <- file(path, "rb")
    on.exit(close(con))
    seek(con, hextoi(start))
    size <- hextoi(end) - hextoi(start)
    readBin(con, what = "raw", n = size)
  }
  
  parse <- function(block, split) {
    split <- charToRaw(split)
    end <- seq_along(block)
    for(i in seq_along(split)) {
      end <- end[block[end + i - 1L] == split[i]]
    }
    end <- c(end + length(split) - 1, length(block))
    begin <- c(1, end[1:length(end) - 1] + 1)
    lapply(seq(begin), function(i) {
      block[begin[i]:end[i]]
    })
  }
  
  parse_block <- function(size = 16, split) {
    block <- read_block(size)
    parse(block, split)
  }
  
  parse_between <- function(start, end, split) {
    block <- read_between(start, end)
    parse(block, split)
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

#' Read a block of data of specified size
#' 
#' Read a block of binary data of a specified size starting at a given offset.
#' 
#' @param path File path.
#' @param offset Integer or character string representing a hexadecimal number
#'   to set the initial position of binary file connection.
#' @param size Number of bytes.
#' @return Vector of type `raw`.
#' @export
read_hex_block <- function(path, offset, size = 16) {
  reader <- hex_reader(path, offset)
  reader$read_block(size)
}

#' Read a block of data between addresses
#' 
#' Read a block of binary data between two addresses.
#' 
#' @param path File path.
#' @param start Integer or character string representing a hexadecimal number
#'   to set the initial position of binary file connection.
#' @param end Integer or character string representing a hexadecimal number that
#'   signifies the address up to which the file is read.
#' @return Vector of type `raw`.
#' @export
read_hex_between <- function(path, start, end) {
  reader <- hex_reader(path)
  reader$read_between(start, end)
}

#' Parse a block of data of specified size
#' 
#' Read a block of binary data of a specified size starting at a given offset
#' and split it into chunks according to the matches to byte sequence `split`
#' within them.
#' 
#' @param path File path.
#' @param offset Integer or character string representing a hexadecimal number
#'   to set the initial position of binary file connection.
#' @param size Number of bytes.
#' @param split Sequence of bytes represented as a character string, eg.
#'   `"\\x50"` or `"\\x50\\x51"`.
#' @return A list of the same length as the number of splits of the byte
#'   sequence.
#' @export
parse_hex_block <- function(path, offset, size = 16, split) {
  reader <- hex_reader(path, offset)
  reader$parse_block(size, split)
}

#' Parse a block of data between addresses
#' 
#' Read a block of binary data between two addresses and split it into chunks
#' according to the matches to byte sequence `split` within them.
#' 
#' @param path File path.
#' @param start Integer or character string representing a hexadecimal number
#'   to set the initial position of binary file connection.
#' @param end Integer or character string representing a hexadecimal number that
#'   signifies the address up to which the file is read.
#' @param split Sequence of bytes represented as a character string, eg.
#'   `"\\x50"` or `"\\x50\\x51"`.
#' @return A list of the same length as the number of splits of the byte
#'   sequence.
#' @export
parse_hex_between <- function(path, start, end, split) {
  reader <- hex_reader(path)
  reader$parse_between(start, end, split)
}
