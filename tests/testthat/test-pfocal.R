base_image <- mtcars$mpg %*%t( mtcars$cyl)
kernel <- chebyshev_distance_kernel(5)
base_image[10,10] <- NA

test_that("emulating mean works", {
  pfocal_results <- 
    pfocal::pfocal(data = base_image, kernel = kernel, 
                   edge_value = 0, na.rm = NA,
                   transform_function = "MULTIPLY",
                   reduce_function = "SUM", 
                   mean_divider = "KERNEL_COUNT")
  raster_focal_results <- 
    raster::as.matrix(raster::focal(x = raster::raster(base_image), w = kernel, 
                                    fun = mean, pad = TRUE, padValue = 0))
  expect_equal(pfocal_results, raster_focal_results)
  
  pfocal_results <- 
    pfocal::pfocal(data = base_image, kernel = kernel, 
                   edge_value = NA, na.rm = FALSE,
                   transform_function = "MULTIPLY",
                   reduce_function = "SUM", 
                   mean_divider = "KERNEL_COUNT")
  raster_focal_results <- 
    raster::as.matrix(raster::focal(x = raster::raster(base_image), w = kernel, 
                                    fun = mean, pad = TRUE, padValue = NA))
  expect_equal(pfocal_results, raster_focal_results)
  
  # pfocal_results <- 
  #   pfocal::pfocal(data = base_image, kernel = kernel, 
  #                  edge_value = NA, na.rm = TRUE,
  #                  transform_function = "MULTIPLY",
  #                  reduce_function = "SUM", 
  #                  mean_divider = "KERNEL_COUNT")
  # raster_focal_results <- 
  #   raster::as.matrix(raster::focal(x = raster::raster(base_image), w = kernel, 
  #                                   fun = mean, pad = TRUE, padValue = NA, na.rm=T))
  # expect_equal(pfocal_results, raster_focal_results)
})
