test_that("distance kernels work as expected", {
  
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

test_that("circle kernels work as expected", {
  
  r <- 2
  
  hard <- matrix(c(0, 0, 1, 0, 0,
                   0, 1, 1, 1, 0,
                   1, 1, 1, 1, 1,
                   0, 1, 1, 1, 0,
                   0, 0, 1, 0, 0), nrow = 5, ncol = 5, byrow = TRUE)
  
  smooth <- matrix(c(0.0000000, 0.2138283, 0.4789669, 0.2138283, 0.0000000,
                     0.2138283, 0.9849691, 1.0000000, 0.9849691, 0.2138283,
                     0.4789669, 1.0000000, 1.0000000, 1.0000000, 0.4789669,
                     0.2138283, 0.9849691, 1.0000000, 0.9849691, 0.2138283,
                     0.0000000, 0.2138283, 0.4789669, 0.2138283, 0.0000000), 
                   nrow = 5, ncol = 5, byrow = TRUE)
  
  expect_equal(hard_uniform_circle_kernel(r), hard, tolerance = 1e-7)
  expect_equal(smooth_uniform_circle_kernel(r), smooth, tolerance = 1e-7)
  
  expect_equal(sum(hard_uniform_circle_kernel(r)), 13)
  expect_equal(sum(smooth_uniform_circle_kernel(r)), 12.5663706)
  
  smooth_one_half <- matrix(c(0, 0.0000000, 0,
                              0, 0.7853982, 0,
                              0, 0.0000000, 0), 
                            nrow = 3, ncol = 3, byrow = TRUE)
  expect_equal(smooth_uniform_circle_kernel(1/2), smooth_one_half, tolerance = 1e-7)
  
  smooth_under_sqrtof2 <- matrix(c(0.0000000, 0.1348958, 0.0000000,
                                   0.1348958, 0.9997973, 0.1348958,
                                   0.0000000, 0.1348958, 0.0000000), 
                                 nrow = 3, ncol = 3, byrow = TRUE)
  expect_equal(smooth_uniform_circle_kernel(0.7), smooth_under_sqrtof2, tolerance = 1e-7)
})
