% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean.R
\name{boundary_cimg}
\alias{boundary_cimg}
\title{Get binarized cimg}
\usage{
boundary_cimg(x3p_path, mask_path, downsample_m, color = "W")
}
\arguments{
\item{x3p_path}{A path to x3p}

\item{mask_path}{A path to the mask corresponding to the x3p file}

\item{downsample_m}{The downsample size}

\item{color}{color of the cimg, default to "W", which is non-na as white}
}
\description{
Convert the x3p and its mask to a cimg
}
