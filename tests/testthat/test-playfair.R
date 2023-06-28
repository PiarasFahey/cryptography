test_that("generating encryption matrix from encryption key works", {
  expect_equal(KeyMatrix("safety first"),
               matrix(c("S","A","F","E","T","Y","I","R","B","C","D","G","H","K","L","M","N","O","P","Q","U","V","W","X","Z"),
                      ncol = 5, nrow = 5, byrow = TRUE))
  expect_equal(KeyMatrix("&*(Spe^C IA L"),
               matrix(c("S","P","E","C","I","A","L","B","D","F","G","H","K","M","N","O","Q","R","T","U","V","W","X","Y","Z"),
                      ncol = 5, nrow = 5, byrow = TRUE))
  })

test_that("encryption works for digraphs of each possible algorithm", {
  expect_equal(playfair("goaeym", "safety first", encrypt = TRUE),"HNFTDU")
  expect_equal(playfair("suyc", "safety first", encrypt = TRUE), "YSIY")
})

test_that("encryption works for non-[a-zA-Z] characters", {
  expect_equal(playfair("go£$%^ae&*()ym", "safe$%ty fir$%^&st(*&^", encrypt = TRUE), "HNFTDU")
  expect_equal(playfair("s£$%^u 19y_{}@@c", "safety first", encrypt = TRUE), "YSIY")
})

test_that("decryption works for digraphs of each possible algorithm", {
  expect_equal(playfair("hnftdu", "safety first", encrypt = FALSE), "GOAEYM")
  expect_equal(playfair("YSIY", "safety first", encrypt = FALSE), "SUYC")
})

test_that("decryption works for non-[a-zA-Z] characters", {
  expect_equal(playfair("H^&N4091F{@~Td$%^U", "safe$%ty fir$%^&st(*&^", encrypt = FALSE), "GOAEYM")
})

test_that("function works for x and z at end of input message", {
  expect_equal(playfair("testingxxz", "key", encrypt = TRUE), "QBTPLONAZU")
  expect_equal(playfair("QBTPLONAZU", "key", encrypt = FALSE), "TESTINGXXZ")
})

test_that("function works with multiple repeated letters for message input", {
  expect_equal(playfair("nnnnalkjstttx", "key", encrypt = TRUE), "SASASASGIENPSZSZSZ")
})
