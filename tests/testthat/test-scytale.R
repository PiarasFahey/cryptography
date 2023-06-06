test_that("encryption and decryption works when length of text is divisible by columns", {
  expect_equal(scytale("very secret message!", 4, encrypt = TRUE), "v rmaeseegretseyc s!")
  expect_equal(scytale("v rmaeseegretseyc s!", 4, encrypt = FALSE), "very secret message!")
})

test_that("encryption and decrytion works when length of text is not divisible by columns", {
  expect_equal(scytale("very secret message", 4, encrypt = TRUE), "v rmaeseegretseyc s")
  expect_equal(scytale("v rmaeseegretseyc s", 4, encrypt = FALSE), "very secret message")
})

test_that("function works on itself with encryption and decryption options", {
  expect_equal(scytale(scytale("Do not tell them the secret!", col = 3, encrypt = TRUE),col = 3, encrypt = FALSE), "Do not tell them the secret!")
})

test_that("input text must be a character vector", {
  expect_error(scytale(c(),3))
  expect_error(scytale(matrix(c("a","b","c","d"),2,2),3))
})

test_that("column parameter must be a positive integer", {
  expect_error(scytale("very secret message!", 1.56))
  expect_error(scytale("very secret message!", "a"))
  expect_error(scytale("very secret message!", -3))
})
