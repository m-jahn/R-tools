# Specific growth rate
library(grofit)
library(nlstools)

SpecGrowthRate <- function(time, data) {
    GroFitOptions <- grofit.control(suppress.messages = TRUE, interactive = FALSE, log.x.gc=FALSE, log.y.gc=FALSE )
    fit <- gcFitSpline(time, data, control=GroFitOptions)
    print(paste("µ=", fit$parameters$mu[[1]], "A=", fit$parameters$A[[1]]))
    return(fit$parameters$mu[[1]])
}

baranyi_fun <- function(LOG10N0, LOG10Nmax, mumax, lag, t) 
    {LOG10Nmax + log10((-1 + exp(mumax * lag) + exp(mumax * t))/(exp(mumax * t) - 1 + exp(mumax * lag) * 10^(LOG10Nmax - LOG10N0)))}


gompertzm_fun <- function(LOG10N0, LOG10Nmax, mumax, lag, t) 
    {LOG10N0 + (LOG10Nmax - LOG10N0) * exp(-exp(mumax * exp(1) * (lag - t)/((LOG10Nmax - LOG10N0) * log(10)) + 1))}



# OD600-correction function for plate reader measurements
# Based on dilution series in 130117_PlateReader, where linearity of photometer and plate reader was compared
OD_corr <- function(OD) {2^((log2(OD)-0.3667669)/0.8613216+0.9411063)}
