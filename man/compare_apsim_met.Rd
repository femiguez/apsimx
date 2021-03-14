% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_apsim_met.R
\name{compare_apsim_met}
\alias{compare_apsim_met}
\alias{plot.met_mrg}
\title{Compare two or more metfiles}
\usage{
compare_apsim_met(
  ...,
  met.var = c("all", "radn", "maxt", "mint", "rain", "rh", "wind_speed", "vp"),
  labels,
  check = FALSE
)

\method{plot}{met_mrg}(
  x,
  ...,
  plot.type = c("vs", "diff", "ts", "density"),
  pairs = c(1, 2),
  cumulative = FALSE,
  met.var = c("radn", "maxt", "mint", "rain"),
  id,
  span = 0.75
)
}
\arguments{
\item{...}{met file objects. Should be of class \sQuote{met}}

\item{met.var}{meteorological variable to plot}

\item{labels}{labels for plotting and identification of \sQuote{met} objects.}

\item{check}{whether to check met files using \sQuote{check_apsim_met}.}

\item{x}{object of class \sQuote{met_mrg}}

\item{plot.type}{either \sQuote{vs}, \sQuote{diff}, \sQuote{ts} - for time series or \sQuote{density}}

\item{pairs}{pair of objects to compare, defaults to 1 and 2 but others are possible}

\item{cumulative}{whether to plot cumulative values (default FALSE)}

\item{id}{identification (not implemented yet)}

\item{span}{argument to be passed to \sQuote{geom_smooth}}
}
\value{
object of class \sQuote{cmet}, which can be used for further plotting
}
\description{
Helper function which allows for a simple comparison among help files

plotting function for compare_apsim_met, it requires ggplot2
}
\note{
I have only tested this for 2 or 3 objects. The code is set up to be able to 
compare more, but I'm not sure that would be all that useful.
}
\examples{
\dontrun{
require(nasapower)
## Specify the location
lonlat <- c(-93, 42)
## dates
dts <- c("2017-01-01","2017-12-31")
## Get pwr
pwr <- get_power_apsim_met(lonlat = lonlat, dates = dts)
## Get data from IEM
iem <- get_iem_apsim_met(lonlat = lonlat, dates = dts)
## Compare them
cmet <- compare_apsim_met(pwr[,1:6], iem, labels = c("pwr","iem"))
## Visualize radiation
plot(cmet, met.var = "radn")
plot(cmet, plot.type = "diff")
plot(cmet, plot.type = "ts")
## Visualize maxt
plot(cmet, met.var = "maxt")
plot(cmet, met.var = "maxt", plot.type = "diff")
plot(cmet, met.var = "maxt", plot.type = "ts")
## Cumulative rain
plot(cmet, met.var = "rain", plot.type = "ts", cumulative = TRUE)
}

}