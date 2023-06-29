
test_that("function works when key is alphabetically ordered and length of input is divisible by length of key", {
  expect_equal(columnar_transposition("testing message", key  = "abc", encrypt = TRUE), "ttgeaei sgsnmse")
  expect_equal(columnar_transposition("ttgeaei sgsnmse", key  = "abc", encrypt = FALSE), "testing message")
  expect_equal(columnar_transposition(columnar_transposition("testing message", key  = "abc"), key = "abc", encrypt = FALSE), "testing message")
})

test_that("function works when key is not alphabetically order and length of input is divisible by length of key", {
  expect_equal(columnar_transposition("testing message", key  = "cab", encrypt = TRUE), "ei sgsnmsettgea")
  expect_equal(columnar_transposition("ei sgsnmsettgea", key  = "cab", encrypt = FALSE), "testing message")
})

test_that("function works when key is alphabetically ordered and length of input is not divisible by length of key", {
  expect_equal(columnar_transposition("testing basic", key = "adhkz", encrypt = TRUE), "tnsegis ctbia")
  expect_equal(columnar_transposition("tnsegis ctbia", key = "adhkz", encrypt = FALSE), "testing basic")
})

test_that("function works when key is not alphabetically ordered and length of input is not divisible by length of key", {
  expect_equal(columnar_transposition("secretmessage", key = "sham", encrypt = TRUE), "cmaetsregsese")
  expect_equal(columnar_transposition("cmaetsregsese", key = "sham", encrypt = FALSE), "secretmessage")
})

test_that("function is symmetric", {
  expect_equal(columnar_transposition(columnar_transposition("secret message123", "safe"), "safe", encrypt = FALSE), "secret message123")
})

test_that("input text must be a character vector", {
  expect_error(columnar_transposition(c(), "key"))
  expect_error(columnar_transposition(matrix(c("a","b","c","d"),2,2), "key"))
})

test_that("key must be an [a-zA-Z] character vector", {
  expect_error(columnar_transposition("very secret message!", 1.56))
  expect_error(columnar_transposition("very secret message!", "密钥"))
  expect_error(columnar_transposition("very secret message!", -3))
  expect_error(columnar_transposition("very secret message!", 0))
  expect_error(columnar_transposition("very secret message", "check123"))
  expect_error(columnar_transposition("very secret message", "c A"))
})

test_that("encrypt must be logical", {
  expect_error(columnar_transposition("very secret message!", "safe", encrypt = 12))
  expect_error(columnar_transposition("very secret message!", "safe", encrypt = c("TRUE")))
})
