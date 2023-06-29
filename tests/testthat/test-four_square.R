
test_that("encryption works for suitable input characters", {
  expect_equal(four_square("testingforletter", "test", "safe"), "UERRFNCDKTNSRTEU")
  expect_equal(four_square("secondtestforletters", "hackerman", "allyourbasebelongtous"), "UYKCLYUOTKBGPTKPUOSI")
})

test_that("encryption works when discarding unsuitable characters", {
  expect_equal(four_square("123Checking13(*&^", "test", "cipher"), "SBSEGFKB")
  expect_equal(four_square("£$%^&*(testing weird characters  测试中文 ", "test", "check"), "UKRSFODWAIRHSFEQSCUKQR")
  expect_equal(four_square("£$%testing1232",  "test", "key"), four_square("testing", "test", "key"))
})

test_that("decryption works for suitable input characters", {
  expect_equal(four_square("UERRFNCDKTNSRTEU", "test", "safe", encrypt = FALSE), "TESTINGFORLETTER")
  expect_equal(four_square("AKNVOTQHCPAGUHBHNKLDGQOTRX", "attack", "charge", encrypt = FALSE), "ALLYOURBASEAREBELONGGTOUSX")
})

test_that("decryption removes any unsuitable input characters", {
  expect_equal(four_square("UERRFNCDKTNSRTEU", "test", "safe", encrypt = FALSE), four_square("U)(*&ERRFNC$%^&DKTNSRT$%^&@{}~EU", "test", "safe", encrypt = FALSE))
})

test_that("function is symmetric for suitable characters", {
  expect_equal(four_square(four_square("CHECKING", "test", "cipher"), "test", "cipher", encrypt = FALSE), "CHECKING")
})

test_that("message must be a character vector", {
  expect_error(four_square(c("a","b"), "key", "key"))
  expect_error(four_square(matrix("abcd"), "key", "key"))
})

test_that("encryption keys must be character vectors", {
  expect_error(four_square("testing", c("a","b"), "key"))
  expect_error(four_sqyare("testing", "key", c("a","b")))
})

test_that("encrypt must be logical", {
  expect_error(four_square("testing", "key", "key", encrypt = MAYBE))
  expect_error(four_square("testing", "key", "key", encrypt = "key"))
})
