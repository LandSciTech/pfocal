
# Higher level, argument level function calling routine below -------------

pFocal_narrow_transform <- function(f) {
  .pFocal_narrow(f, pFocal_transform_info())
}

pFocal_narrow_reduce <- function(f) {
  .pFocal_narrow(f, pFocal_reduce_info())
}

pFocal_narrow_nan_policy <- function(f) {
  .pFocal_narrow(f, pFocal_nan_policy_info())
}

pFocal_narrow_mean_divisor <- function(f) {
  .pFocal_narrow(f, pFocal_mean_divisor_info())
}

pFocal_narrow_variance <- function(f) {
  .pFocal_narrow(f, pFocal_variance_info())
}

# General routine ---------------------------------------------------------

.pFocal_narrow <- function(f, info) {
  if (is.na(f)) {
    return(NA)
    # stop("F is NA")
  }

  if (length(f) != 1L) {
    stop("F must have a length of 1")
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
