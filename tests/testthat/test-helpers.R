context("Helper functions")

test_that("hexadecimal character strings are converted correctly", {
  prefix_in <- c("0x1", "0x91", "0x7e5", "0xg", "0xx5b", "9x5b")
  prefix_ex <- c(1, 145, 2021, NA, NA, NA)
  expect_equal(hextoi(prefix_in), prefix_ex)
  
  nopre_in <- c("1", "91", "7e5", "g", "x5", "%")
  nopre_ex <- c(1, 145, 2021, NA, NA, NA)
  expect_equal(hextoi(nopre_in), nopre_ex)
  
  int_in <- c(1, 145, 2021, 3.14)
  int_ex <- c(1, 145, 2021, NA)
  expect_equal(hextoi(int_in), int_ex)
})
