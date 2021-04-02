context("Reader")

test_that("binary files can be read block by block", {
  path <- "data/test.bin"
  reader <- hex_reader(path)
  
  read_hex1 <- reader$read_block(16)
  read_char1 <- rawToChar(read_hex1)
  exp_char1 <- "Lorem ipsum dolo"
  expect_equal(read_char1, exp_char1)
  
  read_hex2 <- reader$read_block(32)
  read_char2 <- rawToChar(read_hex2)
  exp_char2 <- "r sit amet, fames sed dapibus, t"
  expect_equal(read_char2, exp_char2)
  
  read_off <- as.integer(reader$offset())
  exp_off <- 48
  expect_equal(read_off, exp_off)
  
  reader$offset(16)
  read_hex3 <- reader$read_block(32)
  read_char3 <- rawToChar(read_hex3)
  exp_char3 <- "r sit amet, fames sed dapibus, t"
  expect_equal(read_char3, exp_char3)
})

test_that("binary blocks are read succesfully", {
  path <- "data/test.bin"
  
  read_hex1 <- read_hex_block(path, 16, "0xABC")
  read_char1 <- rawToChar(read_hex1)
  exp_char1 <- "Porttitor purus "
  expect_equal(read_char1, exp_char1)
  
  read_hex2 <- read_hex_between(path, "0xA60", "0xA71")
  read_char2 <- rawToChar(read_hex2)
  exp_char2 <- "In lorem imperdiet"
  expect_equal(read_char2, exp_char2)
})

test_that("blocks read from binary files are split succesfully", {
  path <- "data/test.bin"
  
  split_hex1 <- parse_hex_block(path, size = 1024, split = "\x0a")
  exp_len1 <- 2
  expect_equal(length(split_hex1), exp_len1)
  
  split_hex2 <- parse_hex_between(path, "0xA40", "0xAA0", split = "\x2e\x20")
  read_char2 <- rawToChar(split_hex2[[2]])
  exp_char2 <- "Ultricies non. "
  expect_equal(read_char2, exp_char2)
})
