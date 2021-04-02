#' Binary file reader
#' 
#' `hex_reader()` provides convenience methods for reading and parsing binary
#' data.
#' 
#' @param path File path.
#' @param size Number of bytes.
#' @param offset Integer or character string representing a hexadecimal number
#'   to set the initial position of binary file connection.
#' @return Interface to methods as list.
#' @export
hex_reader <- function(path, size = 16, offset = 0) {
  env <- environment()
  
  block_size <- size
  size <- function(x = NULL) {
    if(is.null(x)) {
      return(block_size)
    }
    assign("block_size", x, envir = env)
  }
  
  con_offset <- as.hexmode(offset)
  offset <- function(x = NULL) {
    if(is.null(x)) {
      return(con_offset)
    }
    assign("con_offset", as.hexmode(x), envir = env)
  }
  
  read_block <- function(size = block_size, offset = con_offset) {
    con <- file(path, "rb")
    on.exit(close(con))
    seek(con, as.hexmode(offset))
    assign("con_offset", as.hexmode(offset) + size, envir = env)
    readBin(con, what = "raw", n = size)
  }
  
  read_between <- function(start, end) {
    con <- file(path, "rb")
    on.exit(close(con))
    seek(con, as.hexmode(start))
    size <- as.hexmode(end) - as.hexmode(start) + 1
    assign("con_offset", as.hexmode(end), envir = env)
    readBin(con, what = "raw", n = size)
  }
  
  parse_block <- function(size = block_size, offset = con_offset, split) {
    block <- read_block(size)
    hexsplit(block, split)
  }
  
  parse_between <- function(start, end, split) {
    block <- read_between(start, end)
    hexsplit(block, split)
  }
  
  list(
    size = size,
    offset = offset,
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
#' @param size Number of bytes.
#' @param offset Integer or character string representing a hexadecimal number
#'   to set the initial position of binary file connection.
#' @return Vector of type `raw`.
#' @export
read_hex_block <- function(path, size = 16, offset) {
  reader <- hex_reader(path)
  reader$read_block(size, offset)
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
#' @param size Number of bytes.
#' @param offset Integer or character string representing a hexadecimal number
#'   to set the initial position of binary file connection.
#' @param split Sequence of bytes represented as a character string, eg.
#'   `"\\x50"` or `"\\x50\\x51"`.
#' @return A list of the same length as the number of splits of the byte
#'   sequence.
#' @export
parse_hex_block <- function(path, size = 16, offset, split) {
  reader <- hex_reader(path)
  reader$parse_block(size, offset, split)
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
