% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trawl.R
\name{extract_tv80}
\alias{extract_tv80}
\title{Extract measurements from Simrad TV80 measurement file}
\usage{
extract_tv80(file)
}
\arguments{
\item{file}{The file path, optimally derived using fs::dir_ls().}
}
\value{
A data frame with data from the PX sensors, Trawl Eye sensor, and trawl vessel.
}
\description{
Extract measurements from Simrad TV80 measurement file
}
\examples{
extract_tv80("C:/data/TV80/2023/12/11/20231211_142842_measurements.csv")
}
