#' Retrieve kernel and arguments information
#'
#' Different kernels and arguments can be passed to [pfocal]. A set of functions
#' is available to provide all possible values for these arguments.
#'
#' @return
#' An object of type `matrix`: first column shows the possible argument value,
#' second column is used internally and third column describe the argument.
#'
#' @examples
#'
#' # Retrieve info on different arguments
#'
#' pfocal_transform_info()
#' pfocal_reduce_info()
#' pfocal_nan_policy_info()
#' pfocal_mean_divisor_info()
#' pfocal_variance_info()
#'
#' # Higher level, argument level functions calling routine below -------------
#' @export
#' @rdname pfocal-info
pfocal_transform_info <- function() {
  .pfocal_info("transform_info", .p_focal_transform_info_cpp())
}

#' @export
#' @rdname pfocal-info
pfocal_reduce_info <- function() {
  .pfocal_info("reduce_info", .p_focal_reduce_info_cpp())
}

#' @export
#' @rdname pfocal-info
pfocal_nan_policy_info <- function() {
  .pfocal_info("policy_info", .p_focal_nan_policy_info_cpp())
}

#' @export
#' @rdname pfocal-info
pfocal_mean_divisor_info <- function() {
  .pfocal_info("divisor_info", .p_focal_mean_divisor_info_cpp())
}

#' @export
#' @rdname pfocal-info
pfocal_variance_info <- function() {
  .pfocal_info("variance_info", .p_focal_variance_info_cpp())
}

# Helpers -----------------------------------------------------------------

.pfocal_info_cache_env <- new.env()

.pfocal_info_clean <- function(info) {
  matrix(c(info[[1]], 0:(length(info[[1]]) - 1), info[[-1]]),
    nrow = length(info[[1]])
  )
}

# General routine ---------------------------------------------------------

.pfocal_info <- function(name, info) {
  if (exists(name, envir = .pfocal_info_cache_env)) {
    return(get(name, envir = .pfocal_info_cache_env))
  } else {
    o <- .pfocal_info_clean(info)
    assign(name, o, envir = .pfocal_info_cache_env)

    return(o)
  }
}
