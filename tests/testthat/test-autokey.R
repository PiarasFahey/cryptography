
test_that("encryption works properly with suitable characters", {
  expect_equal(autokey("testing12345", key = "abc", encrypt = TRUE), DescTools::Vigenere("testing12345", key = "abctesting12"))
  expect_equal(autokey("TeStINg12345", key = "aBC", encrypt = TRUE), DescTools::Vigenere("TeStINg12345", key = "aBCTeStINg12"))
})

test_that("decryption works properly with suitable characters", {
  expect_equal(autokey("J5KcCVPZfZvx", key = "abc", encrypt = FALSE), "testing12345")

})

test_that("function deals with unsuitable characters by removing them", {
  expect_equal(autokey(autokey("leaving 2 spaces###", key = "key123"), key = "key123", encrypt = FALSE), "leaving2spaces")
})
