% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.R
\name{checkdbcolumns}
\alias{checkdbcolumns}
\title{Control the presence of required columns in the
given database tables}
\usage{
checkdbcolumns(tables = c("raw_flow_yearly", "validated_flow_yearly"))
}
\arguments{
\item{tables}{character vector containing the name of
database tables to check}
}
\description{
Database connection parameters are given by the global option
getOption("tradeflowsDB").
}
