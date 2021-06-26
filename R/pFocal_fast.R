
# Fast kernel specific routines -------------------------------------------

fast_gaussian_radious_pFocal <- 
  function(data, vertical_radious, vertical_sd = 1, 
           horizontal_radious = vertical_radious, horizontal_sd = vertical_sd, 
           tail_included = TRUE, na.rm = NA, mp = TRUE, debug_use_r_implementation = FALSE,
           ..., transform_function = "MULTIPLY", reduce_function = "SUM", 
           mean_divider = "ONE", variance = FALSE){
    
    if(length(list(...))){
      stop(paste0("... must be empty. If you want to set the other paramiters, ",
                  "you must do that by name."))
    }
    
    fast_seperated_pFocal(
      data, list(
        gaussian_kernel_radious(vertical_radious,   vertical_sd,   0, 1, 
                                tail_included = tail_included),
        gaussian_kernel_radious(0, 1, horizontal_radious, horizontal_sd, 
                                tail_included = tail_included)),
      na.rm = na.rm, mp = mp, debug_use_r_implementation = debug_use_r_implementation,
      transform_function = transform_function, reduce_function = reduce_function, 
      mean_divider = mean_divider, variance = variance)
  }

fast_gaussian_confidence_pFocal <- 
  function(data, vertical_r0 = 0.05, vertical_sd = 1, 
           horizontal_r0 = vertical_r0, horizontal_sd = vertical_sd, 
           tail_included = TRUE, na.rm = NA, mp = TRUE, debug_use_r_implementation = FALSE,
           ..., transform_function = "MULTIPLY", reduce_function = "SUM", 
           mean_divider = "ONE", variance = FALSE){
    
    if(length(list(...))){
      stop(paste0("... must be empty. If you want to set the other paramiters, ",
                  "you must do that by name."))
    }
    
    fast_seperated_pFocal(
      data, list(
        gaussian_kernel_confidence(vertical_r0, vertical_sd, 1, 1, tail_included = tail_included),
        gaussian_kernel_confidence(1, 1, horizontal_r0, horizontal_sd, tail_included = tail_included)),
      na.rm = na.rm, mp = mp, debug_use_r_implementation = debug_use_r_implementation,
      transform_function = transform_function, reduce_function = reduce_function, 
      mean_divider = mean_divider, variance = variance)
  }

fast_binomial_pFocal <- 
  function(data, vertical_radious, 
           horizontal_radious = vertical_radious,
           na.rm = NA, mp = TRUE, debug_use_r_implementation = FALSE,
           ..., transform_function = "MULTIPLY", reduce_function = "SUM", 
           mean_divider = "ONE", variance = FALSE){
    
    if(length(list(...))){
      stop(paste0("... must be empty. If you want to set the other paramiters, ",
                  "you must do that by name."))
    }
    
    fast_seperated_pFocal(
      data, list(
        binomial_kernel(vertical_radious, 0),
        binomial_kernel(0, horizontal_radious)),
      na.rm = na.rm, mp = mp, debug_use_r_implementation = debug_use_r_implementation,
      transform_function = transform_function, reduce_function = reduce_function, 
      mean_divider = mean_divider, variance = variance)
  }

fast_abs_rectangle_pFocal <- 
  function(data, height, width = height, value = 1,
           na.rm = NA, mp = TRUE, debug_use_r_implementation = FALSE,
           ..., transform_function = "MULTIPLY", reduce_function = "SUM", 
           mean_divider = "ONE", variance = FALSE){
    
    if(length(list(...))){
      stop(paste0("... must be empty. If you want to set the other paramiters, ",
                  "you must do that by name."))
    }
    
    fast_seperated_pFocal(
      data, list(
        matrix(abs(value), height, 1),
        matrix(abs(value), 1, width)),
      na.rm = na.rm, mp = mp, debug_use_r_implementation = debug_use_r_implementation,
      transform_function = transform_function, reduce_function = reduce_function, 
      mean_divider = mean_divider, variance = variance)
  }

# General routine for lists of kernels ------------------------------------

# Only meant for multiply+sum, with no mean and no variance calculation. 
# Applies all kernels in the list in order. Made for separable kernels

fast_seperated_pFocal <- 
  function(data, kernel_list, na.rm = NA, mp = TRUE, debug_use_r_implementation = FALSE, ..., 
           transform_function = "MULTIPLY", reduce_function = "SUM", mean_divider = "ONE", 
           variance = FALSE){
    
    if(length(list(...))){
      stop(paste0("... must be empty. If you want to set the other paramiters, ",
                  "you must do that by name. This function is not meant for that usecase"))
    }
    
    for(k in kernel_list){
      data <- pFocal(data, k, edge_value = 0, na.rm = na.rm, mp = mp, 
                     debug_use_r_implementation = debug_use_r_implementation, 
                     transform_function = transform_function, 
                     reduce_function = reduce_function, mean_divider = mean_divider, 
                     variance = variance)
    }
    return(data)
  }
