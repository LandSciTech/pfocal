
.pFocal_info_cache_env <- new.env()

.pFocal_info_clean <- function(info){matrix(c(info[[1]], 0:(length(info[[1]])-1), info[[-1]]), nrow = length(info[[1]]))}

.pfocal_info <- function(name, info){
  if(exists(name, envir=.pFocal_info_cache_env)){
    return(get(name, envir=.pFocal_info_cache_env))
  }else{
    o <- .pFocal_info_clean(info)
    assign(name, o, envir=.pFocal_info_cache_env)
    return(o)
  }
}

pFocal_transform_info    <- function() .pfocal_info("transform_info", .p_focal_transform_info_cpp())
pFocal_reduce_info       <- function() .pfocal_info("reduce_info",    .p_focal_reduce_info_cpp())
pFocal_nan_policy_info   <- function() .pfocal_info("policy_info",    .p_focal_nan_policy_info_cpp())
pFocal_mean_divisor_info <- function() .pfocal_info("divisor_info",   .p_focal_mean_divisor_info_cpp())
pFocal_variance_info     <- function() .pfocal_info("variance_info",  .p_focal_variance_info_cpp())
