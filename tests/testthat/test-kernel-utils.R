test_that("kernels are normalized correctly", {
  kernel <-  matrix(c(
    1, 2, 3,
    0, 1, 5,
    2, 3, 4
  ), nrow = 3, ncol = 3, byrow = TRUE)
  
  expect_equal(sum(normalize_kernel(kernel, warning_enabled = TRUE), 
                   na.rm = TRUE), 1)
  expect_equal(sum(normalize_kernel(kernel, warning_enabled = FALSE), 
                   na.rm = TRUE), 1)
  
  expect_error(normalize_kernel(kernel, warning_enabled = "NO"), 
               "warning_enabled must be logical")
  
  kernel_0 <-  matrix(c(
    -1, 0, -1,
    1, 2,  1,
    -1, 0, -1
  ), nrow = 3, ncol = 3, byrow = TRUE)
  
  expect_warning(normalize_kernel(kernel_0, warning_enabled = TRUE),
                 "The kernel's values sum to 0, this cannot be normalized")
  expect_equal(normalize_kernel(kernel_0, warning_enabled = FALSE), kernel_0)
})

test_that("kernel flips work correctly", {
  kernel <-  matrix(c(
    1, 2, 3,
    0, 1, 5,
    2, 3, 4
  ), nrow = 3, ncol = 3, byrow = TRUE)
  
  kernel_h <-  matrix(c(
    3, 2, 1,
    5, 1, 0,
    4, 3, 2
  ), nrow = 3, ncol = 3, byrow = TRUE)
  
  kernel_v <-  matrix(c(
    2, 3, 4,
    0, 1, 5,
    1, 2, 3
  ), nrow = 3, ncol = 3, byrow = TRUE)
  
  kernel_b <-  matrix(c(
    4, 3, 2,
    5, 1, 0,
    3, 2, 1
  ), nrow = 3, ncol = 3, byrow = TRUE)
  
  expect_equal(kernel_flip_horizontal(kernel), kernel_h)
  expect_equal(kernel_flip_vertical(kernel), kernel_v)
  expect_equal(kernel_flip_both(kernel), kernel_b)
})

test_that("quarter kernels are assembled correctly", {
  quart_mat <- matrix(c(5, 3, 3,
                        2, 2, 3,
                        1, 2, 4), nrow = 3, ncol = 3, 
                      byrow = TRUE)
  
  full_mat <- matrix(c(3, 3, 5, 3, 3,
                       3, 2, 2, 2, 3,
                       4, 2, 1, 2, 4,
                       3, 2, 2, 2, 3,
                       3, 3, 5, 3, 3), nrow = 5, ncol = 5, 
                     byrow = TRUE)
  
  expect_equal(.q_kernel_to_kernel(quart_mat, quarter = "NE"), full_mat)
})

test_that("kernel assembler checks for corner cases", {
  # If a vector is passed
  expect_warning(.q_kernel_to_kernel(c(1,2), quarter = "NE"), "qk should be a matrix.")
  expect_length(suppressWarnings(.q_kernel_to_kernel(c(1,2), quarter = "NE")), 3)
  
  # If not numeric
  expect_error(.q_kernel_to_kernel(c("a","b"), quarter = "NE"), "qk must be numeric")
  
  # If incorrect quarter
  expect_error(.q_kernel_to_kernel(matrix(c(1,2,
                                            2,1), nrow = 2, ncol = 2, byrow = TRUE), 
                                   quarter = "NN"), 'quarter must be one of "NE", "NW", "SE", or "SW"')
})

