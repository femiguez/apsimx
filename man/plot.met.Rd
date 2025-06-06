% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apsim_met.R
\name{plot.met}
\alias{plot.met}
\title{Plot method for object of class \sQuote{met}}
\usage{
\method{plot}{met}(
  x,
  ...,
  years,
  met.var,
  plot.type = c("ts", "area", "col", "density", "anomaly"),
  cumulative = FALSE,
  facet = FALSE,
  climatology = FALSE,
  summary = FALSE,
  by = c("year", "month")
)
}
\arguments{
\item{x}{object of class \sQuote{met}}

\item{...}{additional arguments. None used at the moment.}

\item{years}{optional argument to subset years}

\item{met.var}{optional argument to choose a certain variable. By default,
temperature (min and max) is displayed}

\item{plot.type}{type of plot, default is \sQuote{ts} or time-series. 
The options \sQuote{area} and \sQuote{col} are only available when summary = TRUE.
Option \sQuote{density} produces a simple plot. Option \sQuote{anomaly} ignores
argument cumulative is treated as TRUE regardless.}

\item{cumulative}{default is FALSE. Especially useful for \sQuote{rain}.}

\item{facet}{whether to display the years in in different panels (facets). Not implemented yet.}

\item{climatology}{logical (default FALSE). Whether to display the \sQuote{climatology}
which would be the average of the data. 
Ideally, there are at least 20 years in the \sQuote{met} object.}

\item{summary}{whether to plot \sQuote{summary} data. (default FALSE).}

\item{by}{whether to perform summary by \sQuote{year} or \sQuote{month}.}
}
\description{
Some plots are similar to APSIM, others are different
and more useful in some respects
}
\examples{
\donttest{
## Read in and plot a met file
extd.dir <- system.file("extdata", package = "apsimx")
ames <- read_apsim_met("Ames.met", src.dir = extd.dir)
plot(ames, years = 2012:2015)
## Perhaps more informative
plot(ames, years = 2012:2015, cumulative = TRUE)
## for rain
plot(ames, met.var = "rain", years = 2012:2015, cumulative = TRUE)
plot(ames, met.var = "rain", years = 2012:2015, cumulative = TRUE, climatology = TRUE)
plot(ames, met.var = "rain", years = 2012:2015, plot.type = "anomaly")
## It is possible to add ggplot elements
library(ggplot2)
p1 <- plot(ames, met.var = "rain", years = 2012:2015, cumulative = TRUE)
p1 + ggtitle("Cumulative rain for 2012-2015")
}

}
