context("Hexfind")

test_that("indexes of binary sequences are located correctly", {
  path <- "../data/test.bin"
  file <- read_hex_block(path, size = file.size(path), 0)
  
  block_1 <- sapply(c("L", "o", "r", "e", "m"), charToRaw)
  exp_1 <- 1
  expect_equal(hexfind(file, block_1), exp_1)
  
  block_2 <- sapply(c("o", "r", "e", "m"), charToRaw)
  exp_2 <- c(2, 2661)
  expect_equal(hexfind(file, block_2), exp_2)
  
  block_3 <- charToRaw(".")
  test_3 <- length(hexfind(file, block_3))
  exp_3 <- 98
  expect_equal(test_3, exp_3)
})
