context("hexmode")

test_that("integers are coerced to hexmode successfully", {
  int_test1 <- as.integer(c(0, 1, 255))
  test_exp1 <- base::as.hexmode(int_test1)
  test_res1 <- as.hexmode(int_test1)
  expect_equal(test_res1, test_exp1)
  
  int_test2 <- as.integer(c(0, 1, 255, NA, 1023))
  test_exp2 <- base::as.hexmode(int_test2)
  test_res2 <- as.hexmode(int_test2)
  expect_equal(test_res2, test_exp2)
})

test_that("doubles are coerced to hexmode successfully", {
  int_test1 <- c(0, 1, 255)
  test_exp1 <- base::as.hexmode(int_test1)
  test_res1 <- as.hexmode(int_test1)
  expect_equal(test_res1, test_exp1)
  
  int_test2 <- c(0, 1, 255, NA, 3.14, 1023.0)
  test_exp2 <- structure(c(0, 1, 255, NA, NA, 1023), class= "hexmode")
  test_res2 <- as.hexmode(int_test2)
  expect_equal(test_res2, test_exp2)
})

test_that("characters are coerced to hexmode successfully", {
  test_char1 <- c("0", "1", "ff", "0xaa", "0xAA", "aA")
  test_exp1 <- base::as.hexmode(test_char1)
  test_res1 <- as.hexmode(test_char1)
  expect_equal(test_res1, test_exp1)
  
  test_char2 <- c("0", "1", "ff", NA, "3.14", "g", "0xaa", "0xAA", "0xxAA")
  int_vec2 <- c(0, 1, 255, NA, NA, NA, 170, 170, NA)
  test_exp2 <- structure(int_vec2, class= "hexmode")
  test_res2 <- as.hexmode(test_char2)
  expect_equal(test_res2, test_exp2)
})

test_that("raw vectors are coerced to hexmode successfully and vice versa", {
  test_vec1 <- c(0, 1, 255)
  raw_test1 <- as.raw(test_vec1)
  test_exp1 <- structure(test_vec1, class = "hexmode")
  test_res1 <- as.hexmode(raw_test1)
  expect_equal(test_res1, test_exp1)
  
  expect_equal(as.raw(test_exp1), raw_test1)
})
