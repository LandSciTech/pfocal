test_that("pfocal info is returned correctly", {
  expect_true(is.matrix(pfocal_info_transform()))
  expect_true(is.matrix(pfocal_info_reduce()))
  expect_true(is.matrix(pfocal_info_nan_policy()))
  expect_true(is.matrix(pfocal_info_mean_divisor()))
  expect_true(is.matrix(pfocal_info_variance()))
})

test_that("pfocal info has appropriate dimensions", {
  expect_equal(dim(pfocal_info_transform()), c(4, 3))
  expect_equal(dim(pfocal_info_reduce()), c(6, 3))
  expect_equal(dim(pfocal_info_nan_policy()), c(3, 3))
  expect_equal(dim(pfocal_info_mean_divisor()), c(16, 3))
  expect_equal(dim(pfocal_info_variance()), c(2, 3))
})
