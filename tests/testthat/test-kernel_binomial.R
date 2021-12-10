# Prepare comparison data
base_v <- c(1, 10, 45, 120, 210)
full_v <- c(base_v, 252, rev(base_v))
base_mat <- matrix(full_v)

test_that("vertical line kernel is created correctly", {
  expect_equal(binomial_kernel(vertical_radius = 5), base_mat)
})

test_that("when horizontal_radius only is given, throws an error", {
  expect_error(binomial_kernel(horizontal_radius = 5),
               "argument \"vertical_radius\" is missing, with no default")
})

test_that("rectangular line kernel is created correctly", {
  expect_equal(binomial_kernel(vertical_radius = 5, horizontal_radius = 5),
               base_mat %*% t(base_mat))
})

test_that("function stops when radius is < 0", {
  expect_error(binomial_kernel(vertical_radius = -5),
               "radius must be >= 0")
})

test_that("function returns a matrux of 1 when radius is 0", {
  expect_equal(binomial_kernel(vertical_radius = 0), matrix(1))
})

test_that("warning is sent when radius is not an even multiple of 1", {
  expect_warning(binomial_kernel(vertical_radius = 4.5),
                 paste0("radius should be an even multiple of 1."))
})
