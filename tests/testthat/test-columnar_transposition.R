
test_that("function works when key is alphabetically ordered", {
  expect_equal(columnar_transposition("testing basic", key = "adhkz", encrypt = TRUE), "tnsegis ctbia")
  expect_equal(columnar_transposition("tnsegis ctbia", key = "adhkz", encrypt = FALSE), "testing basic")
})
