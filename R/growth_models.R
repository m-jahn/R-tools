#' Baranyi growth model
#' 
#' Selection of growth models
#' 
#' @import nlstools
#' 
#' @param LOG10N0 (numeric) log 10 initial biomass density
#' @param  LOG10Nmax (numeric) log 10 maximum biomass density
#' @param mumax (numeric) maximum specific growth rate
#' @param lag (numeric) lag phase in time units
#' @param t (numeric) discrete time points
#' 
#' @return (numeric) vector of modeled biomass corresponding to input time points
#' 
#' @export
# ------------------------------------------------------------------------------
baranyi_fun <- function(LOG10N0, LOG10Nmax, mumax, lag, t) {
  LOG10Nmax + log10(
    (-1 + exp(mumax * lag) + exp(mumax * t)) / 
    (exp(mumax * t) - 1 + exp(mumax * lag) * 10^(LOG10Nmax - LOG10N0))
  )
}

#' Modified Gompertz growth model
#' 
#' Selection of growth models
#' 
#' @param LOG10N0 (numeric) log 10 initial biomass density
#' @param LOG10Nmax (numeric) log 10 maximum biomass density
#' @param mumax (numeric) maximum specific growth rate
#' @param lag (numeric) lag phase in time units
#' @param t (numeric) discrete time points
#' 
#' @return (numeric) vector of modeled biomass corresponding to input time points
#' 
#' @export
# ------------------------------------------------------------------------------
gompertzm_fun <- function(LOG10N0, LOG10Nmax, mumax, lag, t) {
  LOG10N0 + (LOG10Nmax - LOG10N0) * 
  exp(-exp(mumax * exp(1) * (lag - t)/((LOG10Nmax - LOG10N0) * log(10)) + 1))
}

#' Optical density conversion of plate reader to photometer measurements (OD 600 nm)
#' 
#' Selection of growth models
#' 
#' @param OD optical density measurements in plate reader
#' 
#' @return corrected optical density as obtained in photometer
#' 
#' @export
# ------------------------------------------------------------------------------

# OD600-correction function for plate reader measurements
# Based on dilution series in 130117_PlateReader, where linearity of photometer and plate reader was compared
OD_corr <- function(OD) {2^((log2(OD)-0.3667669)/0.8613216+0.9411063)}
