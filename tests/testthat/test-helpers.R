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

test_that("raw vectors are split correctly", {
  vec_raw <- sapply(c("\x01", "\xaa", "\xff", "\x02", "\x03"), charToRaw)
  vec_input <- unname(vec_raw[c(
    "\x01", "\xaa", "\xff", "\x02", "\xaa", "\xff", "\x03", "\xff", "\xff"
  )])
  
  vec_exp1 <- list(
    unname(vec_raw[c("\x01", "\xaa", "\xff")]),
    unname(vec_raw[c("\x02", "\xaa", "\xff")]),
    unname(vec_raw[c("\x03", "\xff")]),
    unname(vec_raw[c("\xff")])
  )
  expect_equal(hexsplit(vec_input, "\xff"), vec_exp1)
  
  vec_exp2 <- list(
    unname(vec_raw[c("\x01", "\xaa")]),
    unname(vec_raw[c("\xff", "\x02", "\xaa")]),
    unname(vec_raw[c("\xff", "\x03", "\xff", "\xff")])
  )
  expect_equal(hexsplit(vec_input, "\xaa"), vec_exp2)
  
  vec_exp3 <- list(
    unname(vec_raw[c("\x01", "\xaa", "\xff")]),
    unname(vec_raw[c("\x02", "\xaa", "\xff")]),
    unname(vec_raw[c("\x03", "\xff", "\xff")])
  )
  expect_equal(hexsplit(vec_input, "\xaa\xff"), vec_exp3)
})
