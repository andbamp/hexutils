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

test_that("a sequence of bytes is decoded correctly", {
  encoding_char <- list(
    c("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8"),
    c("e", "h", "i", "l", "s", "t", "u", "x")
  )
  encoding_int <- list(hextoi(encoding_char[[1]]), encoding_char[[2]])
  encoding_raw <- list(as.raw(hextoi(encoding_char[[1]])), encoding_char[[2]])
  encoding_dat <- as.data.frame(encoding_char)
  
  block <- as.raw(hextoi(
    c("a0", "a2", "a1", "a8", "a0", "a7", "a6", "a3", "a4", "a5", "bb")
  ))
  
  decoded_vec <- c(NA, "h", "e", "x", NA, "u", "t", "i", "l", "s", NA)
  expect_equal(hexdecode(block, encoding_char), decoded_vec)
  expect_equal(hexdecode(block, encoding_int), decoded_vec)
  expect_equal(hexdecode(block, encoding_raw), decoded_vec)
  expect_equal(hexdecode(block, encoding_dat), decoded_vec)
  
  decoded_col_1 <- "hexutils"
  decoded_res_1 <- hexdecode(block, encoding_char, nomatch = "", collapse = "")
  expect_equal(decoded_res_1, decoded_col_1)
  
  decoded_col_2 <- "!h!e!x!!u!t!i!l!s!"
  decoded_res_2 <- hexdecode(block, encoding_char, nomatch = "", collapse = "!")
  expect_equal(decoded_res_2, decoded_col_2)
  
  decoded_nom_3 <- "!hex!utils!"
  decoded_res_3 <- hexdecode(block, encoding_char, nomatch = "!", collapse = "")
  expect_equal(decoded_res_3, decoded_nom_3)
})
