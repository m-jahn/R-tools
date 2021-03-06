% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/growth_models.R
\name{gompertzm_fun}
\alias{gompertzm_fun}
\title{Modified Gompertz growth model}
\usage{
gompertzm_fun(LOG10N0, LOG10Nmax, mumax, lag, t)
}
\arguments{
\item{LOG10N0}{(numeric) log 10 initial biomass density}

\item{LOG10Nmax}{(numeric) log 10 maximum biomass density}

\item{mumax}{(numeric) maximum specific growth rate}

\item{lag}{(numeric) lag phase in time units}

\item{t}{(numeric) discrete time points}
}
\value{
(numeric) vector of modeled log10 biomass corresponding to input time points
}
\description{
Simulate growth according to the Gompertz modified growth model
}
\examples{

# simulate growth according to the Gompertz modified growth model
# for a growth period of 100 hours
biomass <- gompertzm_fun(
  LOG10N0 = -1, LOG10Nmax = 1,
  mumax = 0.1, lag = 10, t = 0:100)

# plot time versus biomass
plot(0:100, biomass)

}
