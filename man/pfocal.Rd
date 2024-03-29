% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pfocal.R
\name{pfocal}
\alias{pfocal}
\title{Fast, parallel implementation of grid data convolution}
\usage{
pfocal(
  data,
  kernel,
  edge_value = 0,
  transform_function = "MULTIPLY",
  reduce_function = "SUM",
  mean_divider = "ONE",
  variance = FALSE,
  na.rm = NA,
  mp = TRUE,
  debug_use_r_implementation = FALSE,
  ...
)
}
\arguments{
\item{data}{\strong{[matrix-type]} Grid to compute onto.}

\item{kernel}{\strong{[matrix]} Computation kernel (neighborhood).}

\item{edge_value}{\strong{[numeric]} Numeric value, \code{NA} or \code{NaN}.The value to
give at the edge of the kernel matrix when the moving window overflows
the data grid.}

\item{transform_function}{\strong{[character]} The function to apply to the
cell values covered by the kernel. For possible values, see
\code{\link[=pfocal_info_transform]{pfocal_info_transform()}}. Default to \code{"MULTIPLY"}.}

\item{reduce_function}{\strong{[character]} The function to apply to the kernel
values after the function passed in \code{transform_function} has been applied
(the function that summarize the data). For possible values, see
\code{\link[=pfocal_info_reduce]{pfocal_info_reduce()}}. Default to \code{"SUM"}.}

\item{mean_divider}{\strong{[character]} Optional, allows to specify how the
final value at each cell is divided by a value that can be function of
the intermediate data resulting of applying \code{transform_function}. For
possible values, see \code{\link[=pfocal_info_mean_divisor]{pfocal_info_mean_divisor()}}. Default to "ONE" (for
no division).}

\item{variance}{\strong{[logical]} Whether to return the "variance" of the
intermediate values at each point (for more details please see
\code{\link[=pfocal_info_variance]{pfocal_info_variance()}}). Default to \code{FALSE} (just returns the value
at each point).}

\item{na.rm}{\strong{[NA OR logical]} The behavior to adopt for dealing with
missing values, default to \code{NA} (faster option). For possible values see
\code{\link[=pfocal_info_nan_policy]{pfocal_info_nan_policy()}}.}

\item{mp}{\strong{[logical]} Whether to use the open_mp implementation,
default to \code{TRUE}.}

\item{debug_use_r_implementation}{\strong{[logical]} Used for debugging purposes
whether to use the slow R implementation instead of the fast C++ code.
Default to \code{FALSE}.}

\item{...}{None used at the moment
.}
}
\value{
The updated, convoluted grid.
}
\description{
A fast, parallel implementation of convolutions for grid-type
data (matrices, rasters and other grid based objects).
}
\details{
Note that the memory allocation for the output is of size
\code{sizeof(double) * ncol * nrow} and for the intermediate
values, \code{sizeof(double) * (ncol + kernel_ncol) * (nrow + kernel_nrow/2)}.
}
\examples{

data <- matrix(nrow = 10, ncol = 10, data = runif(10 * 10))
kernel <- matrix(1 / 9, nrow = 3, ncol = 3)
pfocal(data = data, kernel = kernel)

}
