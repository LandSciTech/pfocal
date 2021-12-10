test_that("error is thrown when tail is nor logical", {
  expect_error(gaussian_kernel_radius(vertical_radius = 5, 
                                      horizontal_radius = 5,
                                      tail_included = "TRUE"),
               "tail_included must be logical")
})

test_that("error is thrown when radius is negative", {
  expect_error(gaussian_kernel_radius(vertical_radius = -5, 
                                      horizontal_radius = 5),
               "radius must be >= 0")
  expect_error(gaussian_kernel_radius(vertical_radius = 5, 
                                      horizontal_radius = -5),
               "radius must be >= 0")
})

test_that("matrix of 1 is returned when radius is zero", {
  expect_equal(gaussian_kernel_radius(vertical_radius = 0, 
                                      horizontal_radius = 0),
               matrix(1))
})

test_that("sum of kernel is 1 whne tail is included", {
  expect_equal(sum(gaussian_kernel_radius(vertical_radius = 5, 
                                          horizontal_radius = 5)),
               1)
  expect_equal(sum(gaussian_kernel_confidence(vertical_r0 = 0.5,
                                              vertical_sd = 1, 
                                              horizontal_r0 = 0.5, 
                                              horizontal_sd = 1)),
               1)
})
