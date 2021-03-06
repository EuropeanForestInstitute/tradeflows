% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean.R, R/cleancomext.R
\name{extractprices}
\alias{extractprices}
\alias{extractpricew}
\title{Extract median prices at a given geographical aggregation level}
\usage{
extractprices(dtf, grouping = c("flow", "regionreporter", "year",
  "unit"), lowercoef = 0.5, uppercoef = 2, lowerquantile = 0.25,
  upperquantile = 0.75, includeqestimates = TRUE)

extractpricew(dtf, lowercoef = 1, uppercoef = 1, grouping = c("flow",
  "regionreporter", "year", "unit"))
}
\arguments{
\item{dtf}{a dataframe containing all trade flows for one product
and their individual prices}

\item{grouping}{a character vector specifying the grouping variables}

\item{lowercoef}{numeric multiplier of the first quartile to
obtain a lower bound on prices}

\item{uppercoef}{numeric multiplier of the third quartile to
obtain an upper bound on prices}

\item{includeqestimates}{logical TRUE when comtrade quantity estimates
can be included}
}
\description{
Group trade flows by the given geographical aggregation level
(region or subregion) and extract the median prices.
Prices depend on the quantity unit. For some products quantity unit changes
from litre to m3 along the years.

Takes a data frame of tradeflows as input.
}
\examples{
\dontrun{
# tf a data frame of trade flows
price <-  tf \%>\% extractprices()
priceglobal <- tf \%>\%
    extractprices(grouping = c("flow", "year", "unit"))
}
}
