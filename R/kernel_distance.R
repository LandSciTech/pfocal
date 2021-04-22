.vertical_distance_quarter_kernel <- function(vertical_radious, horizontal_radious=vertical_radious){
  if(vertical_radious < 0 ||  horizontal_radious < 0){
    stop("The radious must be >= 0")
  }
  return(matrix(0:ceiling(vertical_radious), ceiling(vertical_radious)+1, ceiling(horizontal_radious)+1))
}

.horizontal_distance_quarter_kernel <- function(vertical_radious, horizontal_radious=vertical_radious) t(.vertical_distance_quarter_kernel(horizontal_radious, vertical_radious))

.minkowski_distance_quarter_kernel <- function(p, vertical_radious, horizontal_radious=vertical_radious){
  if(is.finite(p)){
    return((.horizontal_distance_quarter_kernel(vertical_radious, horizontal_radious)^p + .vertical_distance_quarter_kernel(vertical_radious, horizontal_radious)^p)^(1/p))
  }else if(p > 0){
    return(max(.horizontal_distance_quarter_kernel(vertical_radious, horizontal_radious), .vertical_distance_quarter_kernel(vertical_radious, horizontal_radious)))
  }else{
    return(min(.horizontal_distance_quarter_kernel(vertical_radious, horizontal_radious), .vertical_distance_quarter_kernel(vertical_radious, horizontal_radious)))
  }
}

.manhattan_distance_quarter_kernel <- function(vertical_radious, horizontal_radious=vertical_radious)
  .minkowski_distance_quarter_kernel(1, vertical_radious, horizontal_radious)

.chebyshev_distance_quarter_kernel <- function(vertical_radious, horizontal_radious=vertical_radious)
  .minkowski_distance_quarter_kernel(Inf, vertical_radious, horizontal_radious)

.euclidean_distance_quarter_kernel <- function(vertical_radious, horizontal_radious=vertical_radious)
  .minkowski_distance_quarter_kernel(2, vertical_radious, horizontal_radious)





vertical_distance_kernel <- function(vertical_radious, horizontal_radious=vertical_radious)
  .q_kernel_to_kernel(.vertical_distance_quarter_kernel(vertical_radious, horizontal_radious))

horizontal_distance_kernel <- function(vertical_radious, horizontal_radious=vertical_radious)
  .q_kernel_to_kernel(.horizontal_distance_quarter_kernel(vertical_radious, horizontal_radious))

minkowski_distance_kernel <- function(p, vertical_radious, horizontal_radious=vertical_radious)
  .q_kernel_to_kernel(.minkowski_distance_quarter_kernel(p, vertical_radious, horizontal_radious))

manhattan_distance_kernel <- function(vertical_radious, horizontal_radious=vertical_radious)
  .q_kernel_to_kernel(.manhattan_distance_quarter_kernel(vertical_radious, horizontal_radious))

chebyshev_distance_kernel <- function(vertical_radious, horizontal_radious=vertical_radious)
  .q_kernel_to_kernel(.chebyshev_distance_quarter_kernel(vertical_radious, horizontal_radious))

euclidean_distance_kernel <- function(vertical_radious, horizontal_radious=vertical_radious)
  .q_kernel_to_kernel(.euclidean_distance_quarter_kernel(vertical_radious, horizontal_radious))


#most of the time, when someone wants a distance, this is what they want, so we make it the default
distance_kernel <- euclidean_distance_kernel
