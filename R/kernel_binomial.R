

##@MISC {1154968,
##  TITLE = {Is there an equation that represents the nth row in Pascal&#39;s triangle?},
##      AUTHOR = {Yves Daoust (https://math.stackexchange.com/users/65203/yves-daoust)},
##      HOWPUBLISHED = {Mathematics Stack Exchange},
##      NOTE = {URL:https://math.stackexchange.com/q/1154968 (version: 2015-02-18)},
##      EPRINT = {https://math.stackexchange.com/q/1154968},
##      URL = {https://math.stackexchange.com/q/1154968}
##  }
##

#only balanced for now
.binomial_strip <- function(radious){
  if(radious < 0){
    stop("radious must be >= 0")
  }else if((radious%%1)!=0){
    warning("radious should be an even multiple of 1. It will be ceiling()ed to the next hole number")
    radious <- ceiling(radious)
  }else if(radious == 0){
    return(matrix(1))
  }
  
  line_number = radious*2
  
  output = c(1)
  
  for(i in 1:radious){
    output <- append(output[1]*(line_number-(i-1))/i, output)
  }
  
  matrix(output)
}

.binomial_quarter_kernel <- function(vertical_radious, horizontal_radious=0){
  .binomial_strip(vertical_radious) %*% t(.binomial_strip(horizontal_radious))
}

binomial_kernel <- function(vertical_radious, horizontal_radious=0)
  .q_kernel_to_kernel(.binomial_quarter_kernel(vertical_radious, horizontal_radious))

