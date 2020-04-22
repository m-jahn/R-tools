#' Baranyi growth model
#' 
#' Simulate growth according to the Baranyi growth model
#' 
#' @import nlstools
#' 
#' @param LOG10N0 (numeric) log 10 initial biomass density
#' @param  LOG10Nmax (numeric) log 10 maximum biomass density
#' @param mumax (numeric) maximum specific growth rate
#' @param lag (numeric) lag phase in time units
#' @param t (numeric) discrete time points
#' 
#' @return (numeric) vector of modeled log10 biomass corresponding to input time points
#' 
#' @examples
#' 
#' # simulate growth according to the Baranyi growth model
#' # for a growth period of 100 hours
#' biomass <- baranyi_fun(
#'   LOG10N0 = -1, LOG10Nmax = 1,
#'   mumax = 0.1, lag = 10, t = 0:100)
#' 
#' # plot time versus biomass
#' plot(0:100, biomass)
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
#' Simulate growth according to the Gompertz modified growth model
#' 
#' @param LOG10N0 (numeric) log 10 initial biomass density
#' @param LOG10Nmax (numeric) log 10 maximum biomass density
#' @param mumax (numeric) maximum specific growth rate
#' @param lag (numeric) lag phase in time units
#' @param t (numeric) discrete time points
#' 
#' @return (numeric) vector of modeled log10 biomass corresponding to input time points
#' 
#' @examples
#' 
#' # simulate growth according to the Gompertz modified growth model
#' # for a growth period of 100 hours
#' biomass <- gompertzm_fun(
#'   LOG10N0 = -1, LOG10Nmax = 1,
#'   mumax = 0.1, lag = 10, t = 0:100)
#' 
#' # plot time versus biomass
#' plot(0:100, biomass)
#' 
#' @export
# ------------------------------------------------------------------------------
gompertzm_fun <- function(LOG10N0, LOG10Nmax, mumax, lag, t) {
  LOG10N0 + (LOG10Nmax - LOG10N0) * 
  exp(-exp(mumax * exp(1) * (lag - t)/((LOG10Nmax - LOG10N0) * log(10)) + 1))
}
