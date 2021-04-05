context("Hexfind")

test_that("indexes of binary sequences are located correctly", {
  path <- "data/test.bin"
  file <- read_hex_block(path, size = file.size(path), 0)
  
  block_1 <- sapply(c("L", "o", "r", "e", "m"), charToRaw)
  exp_1 <- 1
  expect_equal(hexfind(file, block_1), exp_1)
  expect_equal(hexfind(file, block_1, cpp = FALSE), exp_1)
  
  block_2 <- sapply(c("o", "r", "e", "m"), charToRaw)
  exp_2 <- c(2, 2661)
  expect_equal(hexfind(file, block_2), exp_2)
  expect_equal(hexfind(file, block_2, cpp = FALSE), exp_2)
  
  block_3 <- charToRaw(".")
  test_3 <- length(hexfind(file, block_3))
  exp_3 <- 98
  expect_equal(test_3, exp_3)
  
  test_3b <- length(hexfind(file, block_3, cpp = FALSE))
  expect_equal(test_3b, exp_3)
})

test_that("no match returns integer(0)", {
  path <- "data/test.bin"
  file <- read_hex_block(path, size = file.size(path), 0)
  
  block <- charToRaw("@")
  exp <- integer(0)
  expect_equal(hexfind(file, block), exp)
  expect_equal(hexfind(file, block, cpp = FALSE), exp)
})

test_that("block smaller than x returns integer(0)", {
  x <- as.raw(rep(0, 100))
  block <- as.raw(rep(0, 10))
  exp <- integer(0)
  expect_equal(hexfind(block, x), exp)
  expect_equal(hexfind(block, x, cpp = FALSE), exp)
})

test_that("indexes of block of repeated bytes is located in x of repeated bytes", {
  x <- as.raw(rep(0, 100))
  block <- as.raw(rep(0, 10))
  expect_equal(length(hexfind(x, block)), 91)
  expect_equal(length(hexfind(x, block, cpp = FALSE)), 91)
})
