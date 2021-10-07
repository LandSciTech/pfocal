#' Fast, parallel implementation of grid data convolution
#'
#' A fast, parallel implementation of convolutions for grid-type
#' data (matrices, rasters and other grid based objects).
#'
#' @param data **\[matrix-type\]** Grid to compute onto.
#' @param kernel **\[matrix\]** Computation kernel (neighborhood).
#' @param edge_value **\[numeric\]** Numeric value, `NA` or `NaN`.The value to
#'     give at the edge of the kernel matrix when the moving window overflows
#'     the data grid.
#' @param transform_function **\[character\]** The function to apply to the
#'     cell values covered by the kernel. For possible values, see
#'     [pfocal_info_transform()]. Default to `"MULTIPLY"`.
#' @param reduce_function **\[character\]** The function to apply to the kernel
#'     values after the function passed in `transform_function` has been applied
#'     (the function that summarize the data). For possible values, see
#'     [pfocal_info_reduce()]. Default to `"SUM"`.
#' @param mean_divider **\[character\]** Optional, allows to specify how the
#'     final value at each cell is divided by a value that can be function of
#'     the intermediate data resulting of applying `transform_function`. For
#'     possible values, see [pfocal_info_mean_divisor()]. Default to "ONE" (for
#'     no division).
#' @param variance **\[logical\]** Whether to return the "variance" of the
#'     intermediate values at each point (for more details please see
#'     [pfocal_info_variance()]). Default to `FALSE` (just returns the value
#'     at each point).
#' @param na.rm **\[NA OR character\]** The behavior to adopt for dealing with
#'     missing values, default to `NA`. Far possible values see
#'     [pfocal_info_nan_policy()].
#' @param mp **\[logical\]** Whether to use the open_mp implementation,
#'     default to `TRUE`.
#' @param debug_use_r_implementation **\[logical\]** Used for debugging purposes
#'     whether to use the slow R implementation instead of the fass C++ code.
#'     Default to `FALSE`.
#' @param ... None used.
#'
#' @return
#' The updated, convoluted grid.
#'
#' @details
#' Note that the memory allocation for the output is of size
#' `sizeof(double) * ncol * nrow` and for the intermediate
#' values, `sizeof(double) * (ncol + kernel_ncol) * (nrow + kernel_nrow/2)`.
#'
#' @examples
#'
#' data <- matrix(nrow = 10, ncol = 10, data = runif(10 * 10))
#' kernel <- matrix(1 / 9, nrow = 3, ncol = 3)
#' pfocal(data = data, kernel = kernel)
#' 
#' @export
pfocal <- function(data, kernel, edge_value = 0, transform_function = "MULTIPLY",
                   reduce_function = "SUM", mean_divider = "ONE", variance = FALSE,
                   na.rm = NA, mp = TRUE, debug_use_r_implementation = FALSE, ...) {
  if (methods::is(data, "matrix")) {
    return(pfocal.matrix(
      data, kernel, edge_value, transform_function,
      reduce_function, mean_divider, variance,
      na.rm, mp, debug_use_r_implementation, ...
    ))
  } else if (methods::is(data, "RasterLayer")){
    data_matrix <- raster::as.matrix(data)
    data_transformed <- pfocal.matrix(
      data_matrix, kernel, edge_value, transform_function,
      reduce_function, mean_divider, variance,
      na.rm, mp, debug_use_r_implementation, ...
    )
    raster::values(data) <- data_transformed
    return(data)
  }  else if (methods::is(data, "SpatRaster")){
    browser()
    data_matrix <- terra::as.matrix(data)
    data_transformed <- pfocal.matrix(
      data_matrix, kernel, edge_value, transform_function,
      reduce_function, mean_divider, variance,
      na.rm, mp, debug_use_r_implementation, ...
    )
    terra::values(data) <- data_transformed
    return(data)
  } else if (methods::is(data, "stars")) {
    return(pfocal.stars(
      data, kernel, edge_value, transform_function,
      reduce_function, mean_divider, variance,
      na.rm, mp, debug_use_r_implementation, ...
    ))
  } else {
    stop('unsupported type, x must be a "matrix", "RasterLayer" or a "stars"')
  }
}

# Matrix routine ----------------------------------------------------------

pfocal.matrix <- function(data, kernel, edge_value = NA, transform_function = "MULTIPLY",
                          reduce_function = "SUM", mean_divider = "ONE", variance = FALSE,
                          na.rm = NA, mp = TRUE, debug_use_r_implementation = FALSE, ...) {
  .pfocal(
    data, kernel, edge_value, transform_function, reduce_function, mean_divider,
    variance, na.rm, mp, debug_use_r_implementation
  )
}

# Stars object routine ----------------------------------------------------

pfocal.stars <- function(data, ...) {

  # Code from github.com/michaeldorman/starsExtra R/focal2.R:focal2
  template <- data

  input <- starsExtra::layer_to_matrix(template, check = TRUE)

  output <- pfocal.matrix(input, ...)

  # Back to 'stars'

  output <- t(output)
  template[[1]] <- output

  # Return
  return(template)
}

# General routine ---------------------------------------------------------

.pfocal <- function(data, kernel, edge_value = NA, transform_function = "MULTIPLY",
                    reduce_function = "SUM", mean_divider = "ONE", variance = FALSE,
                    na.rm = NA, mp = TRUE, debug_use_r_implementation = FALSE) {
  pfocal_f <- if (debug_use_r_implementation) {
    .p_focal_r
  } else {
    .p_focal_cpp
  }

  if (is.na(na.rm)) {
    na.rm <- "FAST"
  } else if (na.rm) {
    na.rm <- "NA_RM_TRUE"
  } else {
    na.rm <- "NA_RM_FALSE"
  }

  t_index <- pfocal_narrow_transform(transform_function)
  r_index <- pfocal_narrow_reduce(reduce_function)
  n_index <- pfocal_narrow_nan_policy(na.rm)
  m_index <- pfocal_narrow_mean_divisor(mean_divider)
  v_index <- pfocal_narrow_variance(variance)

  if (is.na(v_index)) {
    stop("variance must be logical or 'TRUE' or 'FALSE'")
  }
  if (is.na(m_index)) {
    stop(paste0(
      "Unknown mean_divider. If you don't want to ",
      "divide the output, leave this set to 'ONE'"
    ))
  }
  if (is.na(n_index)) {
    stop(paste0(
      "Unknown na.rm value. It must be logical, or NA",
      " if you want more speed and don't care about",
      " how NA is handeled"
    ))
  }

  if (is.na(t_index)) {
    # special transform cases
    if (toupper(transform_function) == "SUBTRACT") {
      data <- data * -1
      t_index <- pfocal_narrow_transform("ADD")
    } else if (toupper(transform_function) == "L_DIVIDE") {
      data <- 1 / data
      t_index <- pfocal_narrow_transform("MULTIPLY")
    } else if (toupper(transform_function) %in% c("DIVIDE", "R_DIVIDE")) {
      kernel <- 1 / kernel
      t_index <- pfocal_narrow_transform("MULTIPLY")
    } else {
      stop("Unknown transform_function")
    }
  }

  if (is.na(r_index)) {
    if (toupper(reduce_function) == "RANGE") {
      return(pfocal_f(
        data, kernel, edge_value, t_index, pfocal_narrow_reduce("MAX"),
        n_index, m_index, v_index, mp
      ) -
        pfocal_f(
          data, kernel, edge_value, t_index, pfocal_narrow_reduce("MIN"),
          n_index, m_index, v_index, mp
        ))
    } else {
      stop("Unknown reduce_function")
    }
  }

  pf <- pfocal_f(
    data, kernel, edge_value, t_index, r_index, n_index, m_index,
    v_index, mp
  )

  pf[is.nan(pf)] <- NA
  pf[is.infinite(pf)] <- Inf

  return(pf)
}
