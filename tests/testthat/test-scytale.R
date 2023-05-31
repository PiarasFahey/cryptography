test_that("encryption and decryption works when length of text is divisible by columns", {
  expect_equal(scytale("very secret message!", col=4, encrypt = TRUE), "v rmaeseegretseyc s!")
  expect_equal(scytale("v rmaeseegretseyc s!", col=4, encrypt = FALSE), "very secret message!")
})
