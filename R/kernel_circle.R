#' Compute an Circular kernel
#' 
#' Functions to compute a circular kernel.
#' 
#' @param r **\[numeric\]** Circle radius.
#' 
#' @return 
#' A `matrix` corresponding to the kernel.
#' 
#' @export
#' @rdname kernel-circular
#' @aliases kernel-circular
smooth_uniform_circle_kernel <- function(r) {
  .q_kernel_to_kernel(.smooth_uniform_circle_quarter_kernel(r)) 
}

#' @export
#' @rdname kernel-circular
hard_uniform_circle_kernel <- function(r) {
  .q_kernel_to_kernel(.hard_uniform_circle_quarter_kernel(r)) 
}

# Helpers -----------------------------------------------------------------

.half_circle_integral <- function(r, a, b) {
  # This is $$\int_{a}^b \sqrt{r^2 - x^2} dx$$ simplified slightly
  # integrate from a to b for sqrt(r^2 - x^2) dx
  (b*sqrt(r^2-b^2)-a*sqrt(r^2-a^2)+r^2*(atan2(b, sqrt(r^2-b^2))-atan2(a, sqrt(r^2-a^2))))/2
}

.square_covered_portion <- function(r, x, y){
  x <- abs(x)-1
  y <- abs(y)-1
  if (x < y){
    t <- x
    x <- y
    y <- t
  }
  #now 0,0 is the origin, and 0 <= y <= x
  #only needs to be correct if r > 1.5
  if(x == 0){
    # At the origin
    if (r <= (1/2)){
      # The entire circle fits in the cell
      #      |
      #   O--O--O
      #   |  |  |
      # --O-(+)-$--
      #   |  |  |
      #   O--O--O
      #      |
      
      pi*r*r
    }else if(r <= sqrt(2)/2){
      # The circle reaches out by the right term on each of the 4 sides
      #     /-\
      #   O/-X-\$
      #   /  |  \
      #--(X--+--X)--
      #   \  |  /
      #   O\-X-/O
      #     \-/
      pi*r*r-4*(r*r*acos(0.5/r)-(0.5)*sqrt(r*r-0.5*0.5))
    }else{
      # The circle covers the entire origin cell
      #  /   |   \
      # / X--X--X \
      #   |  |  |
      # --X--+--X--
      #   |  |  |
      # \ X--X--X /
      #  \   |   /
      1
    }
  }else if (y == 0){
    # On the axis, but not at the origin
    if(r < (x-0.5)){
      # not touched by the circle
      #
      #\  O-----O
      # \ |     |
      #--)$-----O--
      # / |     |
      #/  O-----O
      #
      0
    }else if(r^2 <= (((x-0.5)^2)+(0.5^2))){
      # Not covering the first pair of corners
      #
      #  \$-----O
      #   \     |
      #---X)----O--
      #   /     |
      #  /O-----O
      #
      crossover <- sqrt(r^2-(x-0.5)^2)
      2*(.half_circle_integral(r, 0, crossover)-crossover*(x-0.5))
    }else if(r <= (x+0.5)){
      # Covering the first pair of corners, but not reaching the next cell over
      #    \
      #   X-\---O
      #   |  \  |
      #---X---)-$--
      #   |  /  |
      #   X-/---O
      #    /
      
      .half_circle_integral(r, -0.5, 0.5)-(x-0.5)
    }else if(r^2 < (((x+0.5)^2)+(0.5^2))){
      # Reaching in to the next cell over, but not covering this cell completely
      #       \
      #   X----\$
      #   |     \
      #---X-----X)-
      #   |     /
      #   X----/O
      #       /
      crossover <- sqrt(r^2-(x+0.5)^2)
      2*(.half_circle_integral(r, crossover, 0.5)-(0.5-crossover)*(x-0.5)+(crossover))
    }else{
      # The cell is covered
      #          \
      #   X-----X \
      #   |     |  \
      #---X-----X---)
      #   |     |  /
      #   X-----X /
      #          /
      1
    }
    
  }else if(r^2 < ((x-0.5)^2+(y-0.5)^2)){
    # not covered at all, or are on the axis
    #
    #   O    O
    #\
    # \
    #  \$    O
    #   \
    0
  }else if(r^2 <= (x-0.5)^2+(y+0.5)^2){
    # only first corner is covered
    # \
    #  \$    O
    #   \
    #    \
    #   X \  O
    #      \
    crossover <- sqrt(r^2-(x-0.5)^2)
    .half_circle_integral(r, y-0.5, crossover)-(crossover-(y-0.5))*(x-0.5)
  }else if(r^2 <= (x+0.5)^2+(y-0.5)^2){
    # First 2 corners are covered
    #   \
    #   X\   O
    #     \
    #      \
    #   x   \$
    #        \
    .half_circle_integral(r, y-0.5, y+0.5)-(x-0.5)
  }else if(r^2 <= (x+0.5)^2+(y+0.5)^2){
    # Three corners are covered
    #      \
    #   X   \$
    #        \
    #         \
    #   X    X \
    #           \
    crossover <- sqrt(r^2-(x+0.5)^2)
    .half_circle_integral(r, crossover, y+0.5)-((y+0.5)-crossover)*(x-0.5)+(crossover-(y-0.5))
  }else{
    #Completely covered
    #         \
    #   X    X \
    #           \
    #            \
    #   X    X    \
    #              \
    1
  }
}

.smooth_uniform_circle_quarter_kernel <- function(r) {
  qmx = matrix(1:(r+1.5), r+1.5, r+1.5)
  qmy = matrix(1:(r+1.5), r+1.5, r+1.5, byrow=TRUE)
  matrix(mapply(.square_covered_portion, r, qmx, qmy), r+1.5, r+1.5)
}

.hard_uniform_circle_quarter_kernel <- function(r){
  +(.euclidean_distance_quarter_kernel(r)<=r)
}
