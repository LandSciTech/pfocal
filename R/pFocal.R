


.pFocal <- function(data, kernel, edge_value = NA, transform_function = "MULTIPLY", reduce_function = "SUM", mean_divider = "ONE", variance=FALSE, na.rm = NA, mp=TRUE, debug_use_r_implementation=FALSE){
  
  pFocal_f <- if(debug_use_r_implementation){.p_focal_r}else{.p_focal_cpp}
  
  if(is.na(na.rm)){
    na.rm <- "FAST"
  }else if(na.rm){
    na.rm <- "NA_RM_TRUE"
  }else{
    na.rm <- "NA_RM_FALSE"
  }
  
  t_index <- pFocal_narrow_transform(transform_function)
  r_index <- pFocal_narrow_reduce(reduce_function)
  n_index <- pFocal_narrow_nan_policy(na.rm)
  m_index <- pFocal_narrow_mean_divisor(mean_divider)
  v_index <- pFocal_narrow_variance(variance)
  
  if(is.na(v_index)){stop("variance must be logical or 'TRUE' or 'FALSE'")}
  if(is.na(m_index)){stop("Unknown mean_divider. If you don't want to divide the output, leave this set to 'ONE'")}
  if(is.na(n_index)){stop("Unknown na.rm value. It must be logical, or NA if you want more speed and don't care about how NA is handeled")}
  
  if(is.na(t_index)){
    #special transform cases
    if(toupper(transform_function) == "SUBTRACT"){
      data <- data*-1
      t_index <- pFocal_narrow_transform("ADD")
      
    }else if(toupper(transform_function) == "L_DIVIDE"){
      data <- 1/data
      t_index <- pFocal_narrow_transform("MULTIPLY")
      
    }else if(toupper(transform_function) %in% c("DIVIDE", "R_DIVIDE")){
      kernel <- 1/kernel
      t_index <- pFocal_narrow_transform("MULTIPLY")
      
    }else{
      stop("Unknown transform_function")
    }
  }
  
  if(is.na(r_index)){
    if(toupper(reduce_function) == "RANGE"){
      return(pFocal_f(data, kernel, edge_value, t_index, pFocal_narrow_reduce("MAX"), n_index, m_index, v_index, mp)-
             pFocal_f(data, kernel, edge_value, t_index, pFocal_narrow_reduce("MIN"), n_index, m_index, v_index, mp))
    }else{
      stop("Unknown reduce_function")
    }
  }
  
  pf = pFocal_f(data, kernel, edge_value, t_index, r_index, n_index, m_index, v_index, mp)
  
  pf[is.nan(pf)] <- NA
  pf[is.infinite(pf)] <- Inf
  
  return(pf)
  
}


pFocal.matrix <- function(data, kernel, edge_value = NA, transform_function = "MULTIPLY", reduce_function = "SUM", mean_divider = "ONE", variance=FALSE, na.rm = NA, mp=TRUE, debug_use_r_implementation=FALSE, ...) .pFocal(data, kernel, edge_value, transform_function, reduce_function, mean_divider, variance, na.rm, mp, debug_use_r_implementation)
  


#pFocal.stars <- function(x, w, fun = "SUM", weight_fun = "MULTIPLY", na_policy="NOTHING_SPECIAL", mean_policy="KERNEL_SIZE", na_flag = NA, mp=TRUE, debug_use_r_implementation=FALSE){
pFocal.stars <- function(data, ...){
  
  #some code from github.com/michaeldorman/starsExtra R/focal2.R:focal2
  if(requireNamespace("starsExtra", quietly = TRUE)){
    
    template <- data
    
    data = starsExtra::check_one_attribute(data)
    data = starsExtra::check_2d(data)
    
    input = layer_to_matrix(template, check = FALSE)
    
    output <- pFocal.matrix(input, ...)
    
    # Back to 'stars'
    
    output = t(output)
    template[[1]] = output
    
    # Return
    return(template)
    
  }else{
    stop("We require package 'starsExtra' to process stars data")
  }
}
pFocal <- function(data, kernel, edge_value = 0, transform_function = "MULTIPLY", reduce_function = "SUM", mean_divider = "ONE", variance=FALSE, na.rm = NA, mp=TRUE, debug_use_r_implementation=FALSE, ...){
  if(is(data, "matrix")){
    return(pFocal.matrix(data, kernel, edge_value, transform_function, reduce_function, mean_divider, variance, na.rm, mp, debug_use_r_implementation, ...))
  }else if(is(data, "stars")){
    return(pFocal.stars(data, kernel, edge_value, transform_function, reduce_function, mean_divider, variance, na.rm, mp, debug_use_r_implementation, ...))
  }else{
    stop('unsupported type, x must be a "matrix" or a "stars"')
  }
}


