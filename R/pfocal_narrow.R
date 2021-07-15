
# Higher level, argument level function calling routine below -------------

pfocal_narrow_transform <- function(f) {
  .pfocal_narrow(f, pfocal_info_transform())
}

pfocal_narrow_reduce <- function(f) {
  .pfocal_narrow(f, pfocal_info_reduce())
}

pfocal_narrow_nan_policy <- function(f) {
  .pfocal_narrow(f, pfocal_info_nan_policy())
}

pfocal_narrow_mean_divisor <- function(f) {
  .pfocal_narrow(f, pfocal_info_mean_divisor())
}

pfocal_narrow_variance <- function(f) {
  .pfocal_narrow(f, pfocal_info_variance())
}

# General routine ---------------------------------------------------------

.pfocal_narrow <- function(f, info) {

  if (length(f) != 1L) {
    stop("F must have a length of 1")
  }
  
  if (is.na(f)) {
    return(NA)
    # stop("F is NA")
  }

  if (is.numeric(f) || is.logical(f)) {
    f <- as.integer(f)
    if (f >= 0 && f < nrow(info)) {
      return(f)
    } else {
      # stop("F is a numeric that is outside the valid range")
      return(NA)
    }
  } else if (is.character(f)) {
    index <- match(toupper(f), info[, 1]) - 1

    # print(info[,1][index+1])
    # if(is.na(index)){
    # stop("F is a character that is not in the set of allowed values")
    # }
    return(index)
  } else {
    return(NA)
    # stop("F must be a numeric or a string")
  }
}
