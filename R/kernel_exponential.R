
.exponential_quarter_kernel <- function(beta=0.2, r0=0.05){
  r = 1
  while(exp(-beta*r)*(-beta*r-1) < -r0){
    r = r+1
  }
  
  distance_qk <- .euclidean_distance_quarter_kernel(r)
  
  circle_qk <- .hard_uniform_circle_quarter_kernel(r)
  
  return(exp(-beta*distance_qk)*circle_qk)
}

exponential_kernel <- function(beta=0.2, r0=0.05) .q_kernel_to_kernel(.exponential_quarter_kernel(beta=beta, r0=r0))