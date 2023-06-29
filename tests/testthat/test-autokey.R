
test_that("encryption works properly with suitable message and key characters", {
  expect_equal(autokey("testing12345", key = "abc", encrypt = TRUE), DescTools::Vigenere("testing12345", key = "abctesting12"))
  expect_equal(autokey("TeStINg12345", key = "aBC", encrypt = TRUE), DescTools::Vigenere("TeStINg12345", key = "aBCTeStINg12"))
})

test_that("decryption works properly with suitable message and key characters", {
  expect_equal(autokey("J5KcCVPZfZvx", key = "abc", encrypt = FALSE), "testing12345")
  expect_equal(autokey("J5KMCWAjlbhb", key = "abcde", encrypt = FALSE), "testing12345")
})

test_that("method is symmetric for inputs with only usable characters", {
  expect_equal(autokey(autokey("testing12345", "ABcDe12", encrypt = TRUE), "ABcDe12", encrypt = FALSE), "testing12345")
})

test_that("function works with invalid message characters", {
  expect_equal(autokey("te%^sting  {}~@12345", key = "abc", encrypt = TRUE), autokey("testing12345", key = "abc", encrypt = TRUE))
})

test_that("message must be a character vector", {
  expect_error(autokey(matrix(c("a","b"), nrow = 1, ncol = 2), "abc"))
})

test_that("function works with invalid key characters", {
  expect_equal(autokey("testing123", "123$%&"), autokey("testing123", "123"))
})

test_that("key must be a character vector with only letters and numbers", {
  expect_error(autokey("testing123", matrix(c("a","b")), encrypt = FALSE))
})

test_that("encrypt must be logical", {
  expect_error(autokey("testing1233", "abc", encrypt = MAYBE))
  expect_error(autokey("testing123", "abc", encrypt = key))
})
