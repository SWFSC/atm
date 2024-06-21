#' Estimate mixed layer depth from CTD profile. This is a trial function that is not yet working. DO NOT USE!
#'
#' @param data Name of dataframe containing depth and temperature data
#'
#' @return A data frame containing vertically integrated backscatter data.
#' @export
estimate_thermocline <- function(data, depth, temperature) {
  # Read CSV file
  tmp <- data.table::fread(filename, sep = ",")
}

# Load CTD data
ctd <- read.csv("data-raw/uctd-data.asc", header = TRUE, sep = "\t")

# Parse out temperature and depth
depth <- -ctd$DepSM
temperature <- ctd$Tnc90C

# Keep data below the surface layer
temperature <- temperature[depth < -10]
depth <- depth[depth < -10]

# Keep data above deep layer
temperature <- temperature[depth > -80]
depth <- depth[depth > -80]

# Fit data to equation and solve for coefficients using nonlinear least squares
# Tu  = Temperature at top of thermocline
# Tb  = Temperature at bottom of thermocline
# D   = Depth of middle of the thermocline
# W   = Width of the thermocline
model <- nls(temperature ~ Tu + (Tb-Tu)/(1+exp((depth-D)/(2*W))),
             start=list(Tu = 15, Tb = 9, D = -30, W = 10),
             control=nls.control(maxiter=500, minFactor=1e-6, warnOnly = TRUE))
# Plot data
# plot(temperature, depth)
# new.data <- data.frame(depth = seq(min(depth),max(depth),len = 100))
# lines(predict(model, newdata = new.data), new.data$depth)
# points(predict(model, newdata = data.frame(depth = coef(model)[3])), coef(model)[3], col = "red", pch = 21, cex = 1.5, bg = "red")
# lines(c(coef(model)[3],coef(model)[3]), c(-40, -20))
