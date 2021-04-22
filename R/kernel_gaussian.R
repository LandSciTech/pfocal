
.gaussian_strip_radious <- function(radious, sd=1, tail_included=TRUE){
  if(!is.logical(tail_included)){
    stop('tail_included must be logical. If true, the long tail of the distrobution is included in the last element of the strip, otherwise it is simply truncated off')
  }else if(radious < 0){
    stop("radious must be >= 0")
  }else if((radious%%1)!=0){
    warning("radious should be an even multiple of 1. It will be ceiling()ed to the next hole number")
    radious <- ceiling(radious)
  }else if(radious == 0){
    return(matrix(1))
  }
  
  if(tail_included){
    dist <- pnorm(-0.5:(radious+0.5), sd=sd, lower.tail=FALSE)
    matrix(dist-append(dist[-1], 0))
  }else{
    dist <- pnorm(-0.5:(radious+1.5), sd=sd, lower.tail=FALSE)
    dist2 <- dist-append(dist[-1], 0)
    matrix(dist2[-length(dist2)])
  }
}


.gaussian_strip_confidence <- function(r0=0.05, sd=1, tail_included=TRUE){
  if(r0 <= 0){
    stop("r0 must be > 0")
  }else if(r0 > 1){
    stop("r0 must be <= 1")
  }

  .gaussian_strip_radious(ceiling(qnorm((r0/2), sd=sd, lower.tail=FALSE)-0.5), sd=sd, tail_included=tail_included)
}

.gaussian_quarter_kernel_radious <- function(vertical_radious, vertical_sd=1, horizontal_radious=vertical_radious, horizontal_sd=vertical_sd, tail_included=TRUE){
  return(.gaussian_strip_radious(vertical_radious,   sd=vertical_sd,   tail_included=tail_included) 
    %*% 
       t(.gaussian_strip_radious(horizontal_radious, sd=horizontal_sd, tail_included=tail_included)))
}


.gaussian_quarter_kernel_confidence <- function(vertical_r0=0.05, vertical_sd=1, horizontal_r0=vertical_r0, horizontal_sd=vertical_sd, tail_included=TRUE){
  return(.gaussian_strip_confidence(r0=vertical_r0,   sd=vertical_sd,   tail_included=tail_included) 
    %*% 
       t(.gaussian_strip_confidence(r0=horizontal_r0, sd=horizontal_sd, tail_included=tail_included)))
}

gaussian_kernel_confidence <- function(
      vertical_r0=0.05, 
      vertical_sd=1, 
      horizontal_r0=vertical_r0, 
      horizontal_sd=vertical_sd, 
      tail_included=TRUE){
  return(.q_kernel_to_kernel(.gaussian_quarter_kernel_confidence(
      vertical_r0=vertical_r0, 
      vertical_sd=vertical_sd, 
      horizontal_r0=horizontal_r0, 
      horizontal_sd=horizontal_sd, 
      tail_included=tail_included)))
}

gaussian_kernel_radious <- function(
    vertical_radious, 
    vertical_sd=1, 
    horizontal_radious=vertical_radious, 
    horizontal_sd=vertical_sd, 
    tail_included=TRUE){
  return(.q_kernel_to_kernel(.gaussian_quarter_kernel_radious(
    vertical_radious, 
    vertical_radious=vertical_radious, 
    horizontal_radious=horizontal_radious, 
    horizontal_sd=horizontal_sd, 
    tail_included=tail_included)))
}
