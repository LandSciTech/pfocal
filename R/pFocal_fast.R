#' Fast methods for common kernel computations
#' 
#' Methods wrapping [pFocal] to implement common kernel computations with 
#' default argument values.
#' 
#' @param vertical_radius **\[numeric\]** The kernel's radius in the vertical
#'     dimension.
#' @param vertical_sd **\[numeric\]** The kernel's standard deviation in the 
#'     vertical dimension.
#' @param vertical_r0 **\[numeric\]** The kernel's r0 (exponential) in the 
#'     vertical dimension.
#' @param horizontal_radius **\[numeric\]**The kernel's radius in the horizontal
#'     dimension.
#' @param horizontal_sd **\[numeric\]** The kernel's standard deviation in the 
#'     horizontal dimension.
#' @param horizontal_r0 **\[numeric\]** The kernel's r0 (exponential) in the 
#'     horizontal dimension.
#' @param tail_included **\[logical\]** TODO
#' @param height **\[numeric\]** For rectangular kernels, the height of the 
#'     rectangle.
#' @param width **\[numeric\]** For rectangular kernels, the width of the 
#'     rectangle.
#' @param value **\[numeric\]** For single value matrices, the value.
#' @param kernel_list **\[list\]** A list of kernels computed from functions in
#'     [kernel-gaussian], [kernel-binomial], [kernel-circular], 
#'     [kernel-distance], [kernel-exponential].
#' @inheritParams pFocal
#' 
#' @return 
#' The updated, convoluted grid.

# Fast kernel specific routines -------------------------------------------

#' @export
#' @rdname pFocal_fast
fast_gaussian_radius_pFocal <- 
  function(data, vertical_radius, vertical_sd = 1, 
           horizontal_radius = vertical_radius, horizontal_sd = vertical_sd, 
           tail_included = TRUE, na.rm = NA, mp = TRUE, debug_use_r_implementation = FALSE,
           ..., transform_function = "MULTIPLY", reduce_function = "SUM", 
           mean_divider = "ONE", variance = FALSE){
    
    if(length(list(...))){
      stop(paste0("... must be empty. If you want to set the other paramiters, ",
                  "you must do that by name."))
    }
    
    fast_seperated_pFocal(
      data, list(
        gaussian_kernel_radius(vertical_radius,   vertical_sd,   0, 1, 
                                tail_included = tail_included),
        gaussian_kernel_radius(0, 1, horizontal_radius, horizontal_sd, 
                                tail_included = tail_included)),
      na.rm = na.rm, mp = mp, debug_use_r_implementation = debug_use_r_implementation,
      transform_function = transform_function, reduce_function = reduce_function, 
      mean_divider = mean_divider, variance = variance)
  }

#' @export
#' @rdname pFocal_fast
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

#' @export
#' @rdname pFocal_fast
fast_binomial_pFocal <- 
  function(data, vertical_radius, 
           horizontal_radius = vertical_radius,
           na.rm = NA, mp = TRUE, debug_use_r_implementation = FALSE,
           ..., transform_function = "MULTIPLY", reduce_function = "SUM", 
           mean_divider = "ONE", variance = FALSE){
    
    if(length(list(...))){
      stop(paste0("... must be empty. If you want to set the other paramiters, ",
                  "you must do that by name."))
    }
    
    fast_seperated_pFocal(
      data, list(
        binomial_kernel(vertical_radius, 0),
        binomial_kernel(0, horizontal_radius)),
      na.rm = na.rm, mp = mp, debug_use_r_implementation = debug_use_r_implementation,
      transform_function = transform_function, reduce_function = reduce_function, 
      mean_divider = mean_divider, variance = variance)
  }

#' @export
#' @rdname pFocal_fast
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

#' @export
#' @rdname pFocal_fast
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
