#' Compute a Gaussian kernel
#'
#' Functions to compute a Gaussian kernel.
#'
#' @inheritParams pfocal_fast_gaussian_radius
#'
#' @return
#' A `matrix` corresponding to the kernel.
#'
#' @examples 
#' 
#' gaussian_kernel_confidence(vertical_r0 = 0.4, vertical_sd = 1, 
#'                            horizontal_r0 = 0.5, horizontal_sd = 2)
#' gaussian_kernel_confidence(vertical_r0 = 0.4, vertical_sd = 1, 
#'                            horizontal_r0 = 0.5, horizontal_sd = 2)
#' 
#' @export
#' @rdname kernel-gaussian
#' @aliases kernel-gaussian
gaussian_kernel_confidence <- function(vertical_r0 = 0.05,
                                       vertical_sd = 1,
                                       horizontal_r0 = vertical_r0,
                                       horizontal_sd = vertical_sd,
                                       tail_included = TRUE) {
  return(.q_kernel_to_kernel(.gaussian_quarter_kernel_confidence(
    vertical_r0 = vertical_r0,
    vertical_sd = vertical_sd,
    horizontal_r0 = horizontal_r0,
    horizontal_sd = horizontal_sd,
    tail_included = tail_included
  )))
}

#' @export
#' @rdname kernel-gaussian
gaussian_kernel_radius <- function(vertical_radius,
                                   vertical_sd = 1,
                                   horizontal_radius = vertical_radius,
                                   horizontal_sd = vertical_sd,
                                   tail_included = TRUE) {
  return(.q_kernel_to_kernel(.gaussian_quarter_kernel_radius(
    vertical_radius,
    vertical_radius = vertical_radius,
    horizontal_radius = horizontal_radius,
    horizontal_sd = horizontal_sd,
    tail_included = tail_included
  )))
}

# Helpers -----------------------------------------------------------------

.gaussian_strip_radius <- function(radius, sd = 1, tail_included = TRUE) {
  if (!is.logical(tail_included)) {
    stop(paste0('tail_included must be logical. If true, the long tail of the ",
                "distribution is included in the last element of the strip, ",
                "otherwise it is simply truncated off'))
  } else if (radius < 0) {
    stop("radius must be >= 0")
  } else if ((radius %% 1) != 0) {
    warning(paste0(
      "radius should be an even multiple of 1. It will be ",
      "ceiling()ed to the next whole number"
    ))
    radius <- ceiling(radius)
  } else if (radius == 0) {
    return(matrix(1))
  }
  
  if (tail_included) {
    dist <- stats::pnorm(-0.5:(radius + 0.5), sd = sd, lower.tail = FALSE)
    matrix(dist - append(dist[-1], 0))
  } else {
    dist <- stats::pnorm(-0.5:(radius + 1.5), sd = sd, lower.tail = FALSE)
    dist2 <- dist - append(dist[-1], 0)
    matrix(dist2[-length(dist2)])
  }
}

.gaussian_strip_confidence <- function(r0 = 0.05, sd = 1, tail_included = TRUE) {
  if (r0 <= 0) {
    stop("r0 must be > 0")
  } else if (r0 > 1) {
    stop("r0 must be <= 1")
  }
  
  .gaussian_strip_radius(ceiling(stats::qnorm((r0 / 2), sd = sd, lower.tail = FALSE) - 0.5),
                         sd = sd, tail_included = tail_included
  )
}

.gaussian_quarter_kernel_radius <- function(vertical_radius, vertical_sd = 1,
                                            horizontal_radius = vertical_radius,
                                            horizontal_sd = vertical_sd,
                                            tail_included = TRUE) {
  return(.gaussian_strip_radius(vertical_radius,
                                sd = vertical_sd,
                                tail_included = tail_included
  )
  %*%
    t(.gaussian_strip_radius(horizontal_radius,
                             sd = horizontal_sd,
                             tail_included = tail_included
    )))
}


.gaussian_quarter_kernel_confidence <- function(vertical_r0 = 0.05, vertical_sd = 1,
                                                horizontal_r0 = vertical_r0,
                                                horizontal_sd = vertical_sd,
                                                tail_included = TRUE) {
  return(.gaussian_strip_confidence(
    r0 = vertical_r0, sd = vertical_sd,
    tail_included = tail_included
  )
  %*%
    t(.gaussian_strip_confidence(
      r0 = horizontal_r0, sd = horizontal_sd,
      tail_included = tail_included
    )))
}
