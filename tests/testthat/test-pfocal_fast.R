base_image <- mtcars$mpg %*%t( mtcars$cyl)
kernel <- chebyshev_distance_kernel(5)
base_image[10,10] <- NA

test_that("emulating mean works", {
  # TODO best tests are needed here
  pfocal_fast_gaussian_radius(data, vertical_radius = 2)
  pfocal_fast_gaussian_confidence(data)
  pfocal_fast_binomial(data, vertical_radius = 2)
  pfocal_fast_abs_rectangle(data, height = 2)
  pfocal_fast_separated(data,
                        kernel_list = list(binomial_kernel(vertical_radius = 2,
                                                           horizontal_radius = 2),
                                           distance_kernel(vertical_radius = 2,
                                                           horizontal_radius = 2)))
})
