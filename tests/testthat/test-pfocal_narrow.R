test_that("narrowing for transform happens as expected", {
  expect_equal(pfocal_narrow_transform(NA), NA)
  expect_equal(pfocal_narrow_transform("MULTIPLY"), 0)
  expect_equal(pfocal_narrow_transform("ADD"), 1)
  expect_equal(pfocal_narrow_transform("R_EXP"), 2)
  expect_equal(pfocal_narrow_transform("L_EXP"), 3)
})

test_that("narrowing for reduce happens as expected", {
  expect_equal(pfocal_narrow_reduce(NA), NA)
  expect_equal(pfocal_narrow_reduce("SUM"), 0)
  expect_equal(pfocal_narrow_reduce("ABS_SUM"), 1)
  expect_equal(pfocal_narrow_reduce("PRODUCT"), 2)
  expect_equal(pfocal_narrow_reduce("ABS_PRODUCT"), 3)
  expect_equal(pfocal_narrow_reduce("MIN"), 4)
  expect_equal(pfocal_narrow_reduce("MAX"), 5)
})

test_that("narrowing for NAN policy happens as expected", {
  expect_equal(pfocal_narrow_nan_policy(NA), NA)
  expect_equal(pfocal_narrow_nan_policy("NA"), 0)
  expect_equal(pfocal_narrow_nan_policy("FALSE"), 1)
  expect_equal(pfocal_narrow_nan_policy("TRUE"), 2)
})

test_that("narrowing for mean divisor happens as expected", {
  expect_equal(pfocal_narrow_mean_divisor(NA), NA)
  expect_equal(pfocal_narrow_mean_divisor("ONE"), 0)
  expect_equal(pfocal_narrow_mean_divisor("KERNEL_SIZE"), 1)
  expect_equal(pfocal_narrow_mean_divisor("KERNEL_COUNT"), 2)
  expect_equal(pfocal_narrow_mean_divisor("KERNEL_SUM"), 3)
  expect_equal(pfocal_narrow_mean_divisor("KERNEL_ABS_SUM"), 4)
  expect_equal(pfocal_narrow_mean_divisor("KERNEL_PROD"), 5)
  expect_equal(pfocal_narrow_mean_divisor("KERNEL_ABS_PROD"), 6)
  expect_equal(pfocal_narrow_mean_divisor("DYNAMIC_COUNT"), 7)
  expect_equal(pfocal_narrow_mean_divisor("DYNAMIC_SUM"), 8)
  expect_equal(pfocal_narrow_mean_divisor("DYNAMIC_ABS_SUM"), 9)
  expect_equal(pfocal_narrow_mean_divisor("DYNAMIC_PROD"), 10)
  expect_equal(pfocal_narrow_mean_divisor("DYNAMIC_ABS_PROD"), 11)
  expect_equal(pfocal_narrow_mean_divisor("DYNAMIC_DATA_SUM"), 12)
  expect_equal(pfocal_narrow_mean_divisor("DYNAMIC_DATA_ABS_SUM"), 13)
  expect_equal(pfocal_narrow_mean_divisor("DYNAMIC_DATA_PROD"), 14)
  expect_equal(pfocal_narrow_mean_divisor("DYNAMIC_DATA_ABS_PROD"), 15)
})

test_that("narrowing for variance happens as expected", {
  expect_equal(pfocal_narrow_variance(NA), NA)
  expect_equal(pfocal_narrow_variance("FALSE"), 0)
  expect_equal(pfocal_narrow_variance("TRUE"), 1)
})

test_that("narrowing routine works as expected", {
  expect_equal(.pfocal_narrow("ABS_SUM", pfocal_info_reduce()), 1)
  expect_equal(.pfocal_narrow(NA, pfocal_info_reduce()), NA)
  expect_error(.pfocal_narrow(c("SUM", "ABS_SUM"), pfocal_info_reduce()), 
               "F must have a length of 1")
  
  # Numericla values
  expect_equal(.pfocal_narrow(1, pfocal_info_reduce()), 1)
  expect_equal(.pfocal_narrow(0, pfocal_info_reduce()), 0)
  expect_equal(.pfocal_narrow(TRUE, pfocal_info_reduce()), 1)
  expect_equal(.pfocal_narrow(3, pfocal_info_reduce()), 3)
  
  # If larger than rows in possibility matrix
  expect_equal(.pfocal_narrow(7, pfocal_info_reduce()), NA)
  expect_equal(.pfocal_narrow("WRONG_STRING", pfocal_info_reduce()), as.numeric(NA))
})
