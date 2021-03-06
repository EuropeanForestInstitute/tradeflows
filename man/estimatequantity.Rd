% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean.R
\name{estimatequantity}
\alias{estimatequantity}
\title{Estimate quantity}
\usage{
estimatequantity(dtf)
}
\arguments{
\item{dtf}{data frame}
}
\description{
The data frame should contain median prices added by the
joinpricecvfbounds function.
}
\details{
Estimate missing quantity
For each trade flow in the given data frame,
compute quantity_cf from the weight using a conversion factor and
comput quantity_up from the trade value using a unit price.
These columns will be available in the data frame output of this function,
but they will not be saved in the validated database.

Split the data frame between flows which have a quantity from those which don't
complete missing quantity from weight when available or from the
tradevalue when the weight is not available
Check upper and lower bounds on price against a table of reference unit prices
In general NA values should be avoided for the
 upper and lower bounds on prices
}
