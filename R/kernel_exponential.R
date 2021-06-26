#' Compute an Exponential kernel
#' 
#' Functions to compute an exponential kernel.
#' 
#' @param beta **\[numeric\]** The kernel's beta parameter.
#' @param r0 **\[numeric\]** The kernel's r0 (exponential).
#' 
#' @return 
#' A `matrix` corresponding to the kernel.
#' 
#' @export
#' @rdname kernel-exponential
exponential_kernel <- function(beta=0.2, r0=0.05) {
  .q_kernel_to_kernel(.exponential_quarter_kernel(beta=beta, r0=r0))
}

# Helpers -----------------------------------------------------------------

.exponential_quarter_kernel <- function(beta=0.2, r0=0.05){
  r = 1
  while(exp(-beta*r)*(-beta*r-1) < -r0){
    r = r+1
  }
  
  distance_qk <- .euclidean_distance_quarter_kernel(r)
  
  circle_qk <- .hard_uniform_circle_quarter_kernel(r)
  
  return(exp(-beta*distance_qk)*circle_qk)
}
