#' Kernel and arguments information
#' 
#' Different kernels and arguments can be passed to [pFocal]. A set of functions
#' is available to provide all possible values for these arguments.
#' 
#' @return 
#' An object of type `matrix`: first column shows the possible argument value, 
#' second column is used internally and third column describe the argument.

# Higher level, argument level functions calling routine below -------------

#' @export
#' @rdname pFocal-info
pFocal_transform_info <- function(){
  .pfocal_info("transform_info", .p_focal_transform_info_cpp())
}

#' @export
#' @rdname pFocal-info
pFocal_reduce_info <- function(){
  .pfocal_info("reduce_info", .p_focal_reduce_info_cpp())
}

#' @export
#' @rdname pFocal-info
pFocal_nan_policy_info <- function(){
  .pfocal_info("policy_info", .p_focal_nan_policy_info_cpp())
}

#' @export
#' @rdname pFocal-info
pFocal_mean_divisor_info <- function(){
  .pfocal_info("divisor_info", .p_focal_mean_divisor_info_cpp())
}

#' @export
#' @rdname pFocal-info
pFocal_variance_info <- function(){
  .pfocal_info("variance_info", .p_focal_variance_info_cpp())
}

# Helpers -----------------------------------------------------------------

.pFocal_info_cache_env <- new.env()

.pFocal_info_clean <- function(info){
  
  matrix(c(info[[1]], 0:(length(info[[1]])-1), info[[-1]]), 
         nrow = length(info[[1]]))
  
}

# General routine ---------------------------------------------------------

.pfocal_info <- function(name, info){
  
  if(exists(name, envir=.pFocal_info_cache_env)){
    
    return(get(name, envir=.pFocal_info_cache_env))
    
  }else{
    
    o <- .pFocal_info_clean(info)
    assign(name, o, envir=.pFocal_info_cache_env)
    
    return(o)
  }
}