test_that("vertical line kernel is created correctly", {
  base_v <- c(1, 10, 45, 120, 210)
  full_v <- c(base_v, 252, rev(base_v))
  expect_equal(binomial_kernel(vertical_radius = 5), matrix(full_v))
})

test_that("when horizontal_radius only is given, throws an error", {
  expect_error(binomial_kernel(horizontal_radius = 5),
               "argument \"vertical_radius\" is missing, with no default")
})

test_that("rectangular line kernel is created correctly", {
  binomial_kernel(vertical_radius = 5, horizontal_radius = 5)
})

test_that("function stops when radius is < 0", {
  binomial_kernel(vertical_radius = -5)
})

test_that("function returns a matrux of 1 when radius is 0", {
  binomial_kernel(vertical_radius = 0)
})

test_that("ceiling message is sent when radius is not divisible by 1", {
  binomial_kernel(vertical_radius = 4.5)
})
