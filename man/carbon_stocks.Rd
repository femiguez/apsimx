% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apsimx_soil_profile.R
\name{carbon_stocks}
\alias{carbon_stocks}
\title{Calculate soil carbon stocks}
\usage{
carbon_stocks(
  x,
  depth = NULL,
  area = c("m2", "ha"),
  method = c("linear", "constant"),
  value = c("numeric", "carbon.stocks"),
  ...
)
}
\arguments{
\item{x}{object of class \sQuote{soil_profile}}

\item{depth}{soil depth (in meters). If missing then the whole soil profile is used.}

\item{area}{either \sQuote{m2} meter squared or \sQuote{ha}.}

\item{method}{interpolation method. Either \sQuote{linear} or \sQuote{constant}.}

\item{value}{whether to return a \sQuote{numeric} or an object of class \sQuote{carbon.stocks}.}

\item{...}{additional arguments passed to internal functions (none used at the moment).}
}
\value{
returns a value with attribute \sQuote{units} and \sQuote{depth}
}
\description{
Calculation of carbon stocks based on an object of class \sQuote{soil_profile}
}
\details{
Function to calculate carbon stocks. The output units depend on the choice of area.
If \sQuote{m2} is used, then the output units will be \sQuote{kg/m2}. If the \sQuote{area}
is \sQuote{ha}, then the output units will be \sQuote{Mg/ha}.

Note that the bulk density (which is needed in the calculation) is
available as part of the \sQuote{soil_profile} object.
}
\examples{
\dontrun{
sp <- apsimx_soil_profile()
carbon_stocks(sp)
carbon_stocks(sp, depth = 0.1)
carbon_stocks(sp, depth = 0.2)
carbon_stocks(sp, depth = 0.3)
carbon_stocks(sp, depth = 0.4)
}
}
