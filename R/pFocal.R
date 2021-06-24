#' Fast, parallel implementation of raster focal calculations
#' 
#' A fast and pluralized implementation of focal and convolutionnal calculation
#' on raster-type data (matrices and other grid based objects).
#' 
#' @param data **\[matrix-type\]** Grid to compute onto.
#' @param kernel **\[matrix\]** Computation kernel (neighborhood).
#' @param edge_value **\[numeric\]** TBD, default to 0.
#' @param transform_function **\[character\]** TBD, default to "MULTIPLY".
#' @param reduce_function **\[character\]** TBD, default to "SUM".
#' @param mean_divider **\[character\]** TBD, default to "ONE".
#' @param variance **\[logical\]** TBD, default to FALSE.
#' @param na.rm **\[NA OR logical\]** TBD, default to NA.
#' @param mp **\[logical\]** TBD, default to TRUE.
#' @param debug_use_r_implementation **\[logical\]** TDB, default to FALSE.
#' @param ... TDB.
#' 
#' @return 
#' The updated matrix-type object.
#' 
#' @examples 
#' 
#' data <- matrix(nrow = 10, ncol = 10, data = runif(10*10))
#' kernel <- matrix(1/9,nrow=3,ncol=3)
#' pFocal(data = data, kernel = kernel)
#' 
#' @export
pFocal <- function(data, kernel, edge_value = 0, transform_function = "MULTIPLY", 
                   reduce_function = "SUM", mean_divider = "ONE", variance=FALSE, 
                   na.rm = NA, mp=TRUE, debug_use_r_implementation=FALSE, ...){
  if(methods::is(data, "matrix")){
    return(pFocal.matrix(data, kernel, edge_value, transform_function, 
                         reduce_function, mean_divider, variance, 
                         na.rm, mp, debug_use_r_implementation, ...))
  }else if(methods::is(data, "stars")){
    return(pFocal.stars(data, kernel, edge_value, transform_function, 
                        reduce_function, mean_divider, variance, 
                        na.rm, mp, debug_use_r_implementation, ...))
  }else{
    stop('unsupported type, x must be a "matrix" or a "stars"')
  }
}

# Matrix routine ----------------------------------------------------------

pFocal.matrix <- function(data, kernel, edge_value = NA, transform_function = "MULTIPLY", 
                          reduce_function = "SUM", mean_divider = "ONE", variance=FALSE, 
                          na.rm = NA, mp=TRUE, debug_use_r_implementation=FALSE, ...) {
  .pFocal(data, kernel, edge_value, transform_function, reduce_function, mean_divider, 
          variance, na.rm, mp, debug_use_r_implementation)
}

# Stars object routine ----------------------------------------------------

# pFocal.stars <- function(x, w, fun = "SUM", weight_fun = "MULTIPLY", 
#                          na_policy="NOTHING_SPECIAL", mean_policy="KERNEL_SIZE", 
#                          na_flag = NA, mp=TRUE, debug_use_r_implementation=FALSE){

pFocal.stars <- function(data, ...){
  
  # Code from github.com/michaeldorman/starsExtra R/focal2.R:focal2
  template <- data
  
  input <- starsExtra::layer_to_matrix(template, check = TRUE)
  
  output <- pFocal.matrix(input, ...)
  
  # Back to 'stars'
  
  output = t(output)
  template[[1]] = output
  
  # Return
  return(template)
}

# General routine ---------------------------------------------------------

.pFocal <- function(data, kernel, edge_value = NA, transform_function = "MULTIPLY", 
                    reduce_function = "SUM", mean_divider = "ONE", variance=FALSE, 
                    na.rm = NA, mp=TRUE, debug_use_r_implementation=FALSE){
  
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
  if(is.na(m_index)){stop(paste0("Unknown mean_divider. If you don't want to ",
                                 "divide the output, leave this set to 'ONE'"))}
  if(is.na(n_index)){stop(paste0("Unknown na.rm value. It must be logical, or NA",
                                 " if you want more speed and don't care about",
                                 " how NA is handeled"))}
  
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
      return(pFocal_f(data, kernel, edge_value, t_index, pFocal_narrow_reduce("MAX"), 
                      n_index, m_index, v_index, mp)-
               pFocal_f(data, kernel, edge_value, t_index, pFocal_narrow_reduce("MIN"), 
                        n_index, m_index, v_index, mp))
    }else{
      stop("Unknown reduce_function")
    }
  }
  
  pf = pFocal_f(data, kernel, edge_value, t_index, r_index, n_index, m_index, 
                v_index, mp)
  
  pf[is.nan(pf)] <- NA
  pf[is.infinite(pf)] <- Inf
  
  return(pf)
  
}
