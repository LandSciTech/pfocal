% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pfocal_info.R
\name{pfocal_info_transform}
\alias{pfocal_info_transform}
\alias{pfocal_info_reduce}
\alias{pfocal_info_nan_policy}
\alias{pfocal_info_mean_divisor}
\alias{pfocal_info_variance}
\title{Retrieve kernel and arguments information}
\usage{
pfocal_info_transform()

pfocal_info_reduce()

pfocal_info_nan_policy()

pfocal_info_mean_divisor()

pfocal_info_variance()
}
\value{
An object of type \code{matrix}: first column shows the possible argument value,
second column is used internally and third column describe the argument.
}
\description{
Different kernels and arguments can be passed to \link{pfocal}. A set of functions
is available to provide all possible values for these arguments.
}
\examples{

# Retrieve info on different arguments

pfocal_info_transform()
pfocal_info_reduce()
pfocal_info_nan_policy()
pfocal_info_mean_divisor()
pfocal_info_variance()

}
