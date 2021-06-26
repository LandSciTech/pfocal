#' Compute an Distance kernel
#' 
#' Functions to compute a distance kernel.
#' 
#' @inheritParams fast_gaussian_radius_pFocal
#' @param p **\[numeric\]** Exponent parameter for the minkowski distance.
#' 
#' @return 
#' A `matrix` corresponding to the kernel.
#' 
#' @export
#' @rdname kernel-distance
#' @aliases kernel-distance
euclidean_distance_kernel <- function(vertical_radius, 
                                      horizontal_radius=vertical_radius){
  .q_kernel_to_kernel(.euclidean_distance_quarter_kernel(vertical_radius, horizontal_radius))
}

#' @export
#' @rdname kernel-distance
manhattan_distance_kernel <- function(vertical_radius, 
                                      horizontal_radius=vertical_radius){
  .q_kernel_to_kernel(.manhattan_distance_quarter_kernel(vertical_radius, horizontal_radius))
}

#' @export
#' @rdname kernel-distance
minkowski_distance_kernel <- function(p, vertical_radius, 
                                      horizontal_radius=vertical_radius){
  .q_kernel_to_kernel(.minkowski_distance_quarter_kernel(p, vertical_radius, horizontal_radius))
}

#' @export
#' @rdname kernel-distance
chebyshev_distance_kernel <- function(vertical_radius, 
                                      horizontal_radius=vertical_radius){
  .q_kernel_to_kernel(.chebyshev_distance_quarter_kernel(vertical_radius, horizontal_radius))
}

#' @export
#' @rdname kernel-distance
vertical_distance_kernel <- function(vertical_radius, 
                                     horizontal_radius=vertical_radius){
  .q_kernel_to_kernel(.vertical_distance_quarter_kernel(vertical_radius, horizontal_radius))
}

#' @export
#' @rdname kernel-distance
horizontal_distance_kernel <- function(vertical_radius, 
                                       horizontal_radius=vertical_radius){
  .q_kernel_to_kernel(.horizontal_distance_quarter_kernel(vertical_radius, horizontal_radius))
}

# Most of the time, when someone wants a distance, this is what they want, 
# so we make it the default
#' @export
#' @rdname kernel-distance
distance_kernel <- euclidean_distance_kernel

# Helpers -----------------------------------------------------------------

.vertical_distance_quarter_kernel <- function(vertical_radius, 
                                              horizontal_radius=vertical_radius){
  if(vertical_radius < 0 ||  horizontal_radius < 0){
    stop("The radius must be >= 0")
  }
  return(matrix(0:ceiling(vertical_radius), ceiling(vertical_radius)+1, 
                ceiling(horizontal_radius)+1))
}

.horizontal_distance_quarter_kernel <- function(vertical_radius, 
                                                horizontal_radius=vertical_radius) {
  t(.vertical_distance_quarter_kernel(horizontal_radius, vertical_radius))
}

.minkowski_distance_quarter_kernel <- function(p, vertical_radius, 
                                               horizontal_radius=vertical_radius){
  if(is.finite(p)){
    return((.horizontal_distance_quarter_kernel(vertical_radius, horizontal_radius)^p + 
              .vertical_distance_quarter_kernel(vertical_radius, horizontal_radius)^p)^(1/p))
  }else if(p > 0){
    return(max(.horizontal_distance_quarter_kernel(vertical_radius, horizontal_radius), 
               .vertical_distance_quarter_kernel(vertical_radius, horizontal_radius)))
  }else{
    return(min(.horizontal_distance_quarter_kernel(vertical_radius, horizontal_radius), 
               .vertical_distance_quarter_kernel(vertical_radius, horizontal_radius)))
  }
}

.manhattan_distance_quarter_kernel <- function(vertical_radius, horizontal_radius=vertical_radius){
  .minkowski_distance_quarter_kernel(1, vertical_radius, horizontal_radius)
}

.chebyshev_distance_quarter_kernel <- function(vertical_radius, horizontal_radius=vertical_radius){
  .minkowski_distance_quarter_kernel(Inf, vertical_radius, horizontal_radius)
}

.euclidean_distance_quarter_kernel <- function(vertical_radius, horizontal_radius=vertical_radius){
  .minkowski_distance_quarter_kernel(2, vertical_radius, horizontal_radius)
}
