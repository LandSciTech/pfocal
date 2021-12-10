test_that("default kernel sum is conserved", {
  expect_equal(sum(exponential_kernel()), 149.48997)
})

test_that("dimensions are correct", {
  expect_equal(dim(exponential_kernel()), c(49, 49))
})
