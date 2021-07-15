test_that("distance kernels work as expcted", {
  
  expect_equal(minkowski_distance_kernel(p = 2, vertical_radius = 2, horizontal_radius = 2), 
               matrix(c(2.828427, 2.236068, 2, 2.236068, 2.828427,
                        2.236068, 1.414214, 1, 1.414214, 2.236068,
                        2.000000, 1.000000, 0, 1.000000, 2.000000,
                        2.236068, 1.414214, 1, 1.414214, 2.236068,
                        2.828427, 2.236068, 2, 2.236068, 2.828427), nrow = 5, ncol = 5, byrow = TRUE),
               tolerance = 1e-7)
  expect_equal(minkowski_distance_kernel(p = 2, vertical_radius = 2, horizontal_radius = 2), 
               euclidean_distance_kernel(vertical_radius = 2, horizontal_radius = 2))
  
  expect_equal(minkowski_distance_kernel(p = 1, vertical_radius = 2, horizontal_radius = 2), 
               matrix(c(4, 3, 2, 3, 4,
                        3, 2, 1, 2, 3,
                        2, 1, 0, 1, 2,
                        3, 2, 1, 2, 3,
                        4, 3, 2, 3, 4), nrow = 5, ncol = 5, byrow = TRUE))
  expect_equal(minkowski_distance_kernel(p = 1, vertical_radius = 2, horizontal_radius = 2), 
               manhattan_distance_kernel(vertical_radius = 2, horizontal_radius = 2))
  
  expect_equal(minkowski_distance_kernel(p = Inf, vertical_radius = 2, horizontal_radius = 2), 
               matrix(c(2, 2, 2, 2, 2,
                        2, 1, 1, 1, 2,
                        2, 1, 0, 1, 2,
                        2, 1, 1, 1, 2,
                        2, 2, 2, 2, 2), nrow = 5, ncol = 5, byrow = TRUE))
  expect_equal(minkowski_distance_kernel(p = Inf, vertical_radius = 2, horizontal_radius = 2), 
               chebyshev_distance_kernel(vertical_radius = 2, horizontal_radius = 2))
  
  expect_equal(minkowski_distance_kernel(p = -Inf, vertical_radius = 2, horizontal_radius = 2), 
               matrix(c(2, 1, 0, 1, 2,
                        1, 1, 0, 1, 1,
                        0, 0, 0, 0, 0,
                        1, 1, 0, 1, 1,
                        2, 1, 0, 1, 2), nrow = 5, ncol = 5, byrow = TRUE))
  
  expect_equal(vertical_distance_kernel(vertical_radius = 2, horizontal_radius = 2), 
               matrix(c(2, 2, 2, 2, 2,
                        1, 1, 1, 1, 1,
                        0, 0, 0, 0, 0,
                        1, 1, 1, 1, 1,
                        2, 2, 2, 2, 2), nrow = 5, ncol = 5, byrow = TRUE))
  
  expect_equal(horizontal_distance_kernel(vertical_radius = 2, horizontal_radius = 2), 
               matrix(c(2, 2, 2, 2, 2,
                        1, 1, 1, 1, 1,
                        0, 0, 0, 0, 0,
                        1, 1, 1, 1, 1,
                        2, 2, 2, 2, 2), nrow = 5, ncol = 5, byrow = FALSE))
  
  expect_error(vertical_distance_kernel(vertical_radius = -1, horizontal_radius = 2),
               "The radius must be >= 0")
})
