context("sample_test_file")

test_that("TRUE is TRUE and not FALSE", {
  expect_equal(TRUE, TRUE)
  expect_equal(TRUE, !FALSE)
})
