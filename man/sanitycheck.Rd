% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean.R
\name{sanitycheck}
\alias{sanitycheck}
\title{Check various features of the trade flows data frame}
\usage{
sanitycheck(dtf)
}
\arguments{
\item{dtf}{data frame of trade flows data}
}
\description{
"Import" and "Export" and used to add partner flow information.
Should these characters be different then the user should be warned.
"Import" and "Export" are hardcoded in the swapreporterpartner() function.
This should be changed to environement variables using options() and getOption().
}
