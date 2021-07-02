#' Normalize a kernel
#'
#' A normalized kernel is one that, if given a data matrix that is all 1s, will
#' result in 1. This function applies a scalier multiple to the kernel to make
#' it normalized, if possible. Otherwise, it optionally warns and returns the
#' input kernel.
#'
#' @param k **\[matrix\]** The kernel.
#' @param warning_enabled **\[logical\]** Whether to unable warnings, default
#'     to `TRUE`.
#'
#' @return
#' The normalized kernel, a `matrix`.
#'
#' @examples
#'
#' kernel <- distance_kernel(vertical_radius = 2, horizontal_radius = 2)
#' kernel_norm <- normalize_kernel(kernel)
#' sum(kernel_norm)
#' 
#' @export
normalize_kernel <- function(k, warning_enabled = TRUE) {
  if (!is.logical(warning_enabled)) {
    stop(paste0(
      "warning_enabled must be logical. If TRUE, and if the kernal ",
      "cannot be normalized (ex: it sums to 0) then a warning will be generated"
    ))
  }

  s <- sum(k, na.rm = TRUE)
  if (s != 0) {
    return(k / s)
  } else {
    if (warning_enabled) {
      warning("The kernel's values sum to 0, this cannot be normalized")
    }
    return(k)
  }
}

# -------------------------------------------------------------------------

#' Flip a kernel
#'
#' Flip the kernel vertically, horizontally, or both.
#'
#' @inheritParams normalize_kernel
#'
#' @return
#' The flipped kernel, a `matrix`.
#'
#' @examples
#'
#' kernel <- matrix(c(
#'   1, 2, 3,
#'   0, 1, 0,
#'   2, 3, 4
#' ), nrow = 3, ncol = 3)
#' kernel_flip_horizontal(kernel)
#' kernel_flip_vertical(kernel)
#' kernel_flip_both(kernel)
#' @export
#' @rdname kernel_flip
kernel_flip_horizontal <- function(k) {
  k[, c(ncol(k):1), drop = FALSE]
}

#' @export
#' @rdname kernel_flip
kernel_flip_vertical <- function(k) {
  k[c(nrow(k):1), , drop = FALSE]
}

#' @export
#' @rdname kernel_flip
kernel_flip_both <- function(k) {
  k[c(nrow(k):1), c(ncol(k):1), drop = FALSE]
}

# Helper ------------------------------------------------------------------

.q_kernel_to_kernel <- function(qk, quarter = "SE") {
  if (!is.matrix(qk)) {
    if (is.numeric(qk)) {
      warning("qk should be a matrix. If it is not, but it is numeric, this assumes that it is a matrix with 1 row")
      qk <- matrix(qk, 1)
    } else {
      stop("qk must be numeric, and should be a matrix")
    }
  }

  quarter_labels <- c("SE", "NE", "NW", "SW")

  quarter <- toupper(quarter)
  if (!(quarter %in% quarter_labels)) {
    stop('quarter must be one of "NE", "NW", "SE", or "SW". This represents which quarter of the kernel you are passing in. "SE" is the default')
  }

  se <- c(identity, kernel_flip_vertical, kernel_flip_both, kernel_flip_horizontal)[[match(quarter, quarter_labels)]](qk)

  height <- nrow(qk)
  width <- ncol(qk)
  extra_height <- height - 1
  extra_width <- width - 1

  output <- matrix(0, height + extra_height, width + extra_width)

  output[1:height, 1:width] <- kernel_flip_both(se)
  output[-(1:extra_height), 1:width] <- kernel_flip_horizontal(se)
  output[1:height, -(1:extra_width)] <- kernel_flip_vertical(se)
  output[-(1:extra_height), -(1:extra_width)] <- se

  return(output)
}
