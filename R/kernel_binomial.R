#' Compute an Binomial kernel
#'
#' Functions to compute a binomial kernel.
#'
#' @inheritParams fast_binomial_pFocal
#'
#' @return
#' A `matrix` corresponding to the kernel.
#'
#' @export
#' @rdname kernel-binomial
#' @aliases kernel-binomial
binomial_kernel <- function(vertical_radius, horizontal_radius = 0) {
  .q_kernel_to_kernel(.binomial_quarter_kernel(vertical_radius, horizontal_radius))
}

# @MISC {1154968,
#   TITLE = {Is there an equation that represents the nth row in Pascal&#39;s triangle?},
#       AUTHOR = {Yves Daoust (https://math.stackexchange.com/users/65203/yves-daoust)},
#       HOWPUBLISHED = {Mathematics Stack Exchange},
#       NOTE = {URL:https://math.stackexchange.com/q/1154968 (version: 2015-02-18)},
#       EPRINT = {https://math.stackexchange.com/q/1154968},
#       URL = {https://math.stackexchange.com/q/1154968}
#   }
#

# Helpers -----------------------------------------------------------------

# Only balanced for now
.binomial_strip <- function(radius) {
  if (radius < 0) {
    stop("radius must be >= 0")
  } else if ((radius %% 1) != 0) {
    warning("radius should be an even multiple of 1. It will be ceiling()ed to the next hole number")
    radius <- ceiling(radius)
  } else if (radius == 0) {
    return(matrix(1))
  }

  line_number <- radius * 2

  output <- c(1)

  for (i in 1:radius) {
    output <- append(output[1] * (line_number - (i - 1)) / i, output)
  }

  matrix(output)
}

.binomial_quarter_kernel <- function(vertical_radius, horizontal_radius = 0) {
  .binomial_strip(vertical_radius) %*% t(.binomial_strip(horizontal_radius))
}
