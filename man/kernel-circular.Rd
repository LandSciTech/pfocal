% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kernel_circle.R
\name{smooth_uniform_circle_kernel}
\alias{smooth_uniform_circle_kernel}
\alias{kernel-circular}
\alias{hard_uniform_circle_kernel}
\title{Compute an Circular kernel}
\usage{
smooth_uniform_circle_kernel(r)

hard_uniform_circle_kernel(r)
}
\arguments{
\item{r}{\strong{[numeric]} Circle radius.}
}
\value{
A \code{matrix} corresponding to the kernel.
}
\description{
Functions to compute a circular kernel.
}
\examples{

smooth_uniform_circle_kernel(r = 3)
hard_uniform_circle_kernel(r = 3)

}
