% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/atm.R
\name{extract_cps_nasc}
\alias{extract_cps_nasc}
\title{Estimate CPS backscatter in Echoview CSV files}
\usage{
extract_cps_nasc(
  path.in,
  pattern.in,
  path.out = path.in,
  suffix.out = "_nasc_cps.csv",
  x11.w = 1600,
  x11.h = 600,
  jpeg = TRUE,
  path.img = NULL,
  pattern.img = NULL,
  dist.bin = 2000,
  max.range = 350,
  transparency = 0.2,
  root = 1.5,
  scaling = 0.1,
  expansion = 2
)
}
\arguments{
\item{path.in}{Directory containing CSV files.}

\item{pattern.in}{Regular expression pattern used to list available CSV files.}

\item{path.out}{Directory where outputs will be stored. Usually the same as \code{path.in}.}

\item{suffix.out}{Suffix to append to resulting CSV file.}

\item{x11.w}{Width of graphics window for line picks. Default = 1600 pixels.}

\item{x11.h}{Height of graphics window for line picks. Default = 600 pixels.}

\item{jpeg}{Save intermediate plots from line picks (TRUE/FALSE)?}

\item{path.img}{Directory containing exported echogram images.}

\item{pattern.img}{Regular expression pattern used to open corresponding echogram image.}

\item{dist.bin}{Distance bin (m) for plotting results. Default is 2000 m, but should be made smaller for short transects.}

\item{max.range}{Maximum depth range for bubble plots.}

\item{root}{Constant for controlling bubble size. Default = 2 and should probably not be changed.}

\item{scaling}{Constant for controlling bubble size. Default = 0.1 and should probably not be changed.}

\item{expansion}{Constant for expanding axes in bubble plots.}
}
\value{
No objects are returned, but a new CSV file is produced along with several plots images showing the processing results.
}
\description{
Estimate CPS backscatter in Echoview CSV files
}
