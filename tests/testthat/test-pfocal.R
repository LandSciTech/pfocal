base_image <- mtcars$mpg %*%t( mtcars$cyl)
the_kernel <- chebyshev_distance_kernel(5)
base_image[10,10] <- NA

test_that("emulating mean works", {
  pfocal_results <- 
    pfocal::pfocal(data = base_image, kernel = the_kernel, 
                   edge_value = 0, na.rm = NA,
                   transform_function = "MULTIPLY",
                   reduce_function = "SUM", 
                   mean_divider = "KERNEL_COUNT")
  raster_focal_results <- 
    raster::as.matrix(raster::focal(x = raster::raster(base_image), w = the_kernel, 
                                    fun = mean, pad = TRUE, padValue = 0))
  expect_equal(pfocal_results, raster_focal_results)
  
  pfocal_results <- 
    pfocal::pfocal(data = base_image, kernel = the_kernel, 
                   edge_value = NA, na.rm = FALSE,
                   transform_function = "MULTIPLY",
                   reduce_function = "SUM", 
                   mean_divider = "KERNEL_COUNT")
  raster_focal_results <- 
    raster::as.matrix(raster::focal(x = raster::raster(base_image), w = the_kernel, 
                                    fun = mean, pad = TRUE, padValue = NA))
  expect_equal(pfocal_results, raster_focal_results)
  
  # pfocal_results <- 
  #   pfocal::pfocal(data = base_image, kernel = the_kernel, 
  #                  edge_value = NA, na.rm = TRUE,
  #                  transform_function = "MULTIPLY",
  #                  reduce_function = "SUM", 
  #                  mean_divider = "KERNEL_COUNT")
  # raster_focal_results <- 
  #   raster::as.matrix(raster::focal(x = raster::raster(base_image), w = the_kernel, 
  #                                   fun = mean, pad = TRUE, padValue = NA, na.rm=T))
  # expect_equal(pfocal_results, raster_focal_results)
})

test_that("spatraster work", {
  expect_match(class(
    pfocal::pfocal(data = terra::rast(base_image), kernel = the_kernel, 
                   edge_value = NA, na.rm = FALSE,
                   transform_function = "MULTIPLY",
                   reduce_function = "SUM", 
                   mean_divider = "KERNEL_COUNT")), "SpatRaster")
  
})

test_that("stars work", {
  expect_match(class(
    pfocal::pfocal(data = stars::st_as_stars(base_image), kernel = the_kernel, 
                   edge_value = NA, na.rm = FALSE,
                   transform_function = "MULTIPLY",
                   reduce_function = "SUM", 
                   mean_divider = "KERNEL_COUNT")), "stars")
  
})

test_that("erroring works", {
  expect_error(pfocal::pfocal(data = base_image, kernel = the_kernel, 
                              variance = NA),
               "variance must be logical or 'TRUE' or 'FALSE'")
  expect_error(pfocal::pfocal(data = base_image, kernel = the_kernel, 
                              mean_divider = NA),
               "Unknown mean_divider.")
  # expect_error(pfocal::pfocal(data = base_image, kernel = the_kernel, 
  #                             na.rm = "y"),
  #              "Unknown mean_divider.")
  expect_error(pfocal::pfocal(data = base_image, kernel = the_kernel, 
                              transform_function = "ANY"),
               "Unknown transform_function")
  expect_error(pfocal::pfocal(data = base_image, kernel = the_kernel, 
                              reduce_function = "ANY"),
               "Unknown reduce_function")
  
})
