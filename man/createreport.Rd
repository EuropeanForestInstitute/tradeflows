% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report.R
\name{createreport}
\alias{createreport}
\title{Create data completeness report for comtrade data}
\usage{
createreport(tfdata, template, fileprefix = NULL, filesuffix = NULL,
  productcodeinreport = NULL, reporterinreport = NULL,
  inputpath = system.file("templates", package = "tradeflows"),
  outputdir = "reports", encoding = "UTF-8", toc = TRUE,
  keep_tex = FALSE)
}
\arguments{
\item{tfdata}{a dataframe containing trade flows data that
will be passed to R code run by the template}

\item{template}{name of the template file, including .Rmd extension}

\item{fileprefix}{character string at the begining of the generated pdf file name}

\item{productcodeinreport}{numeric or character string leave this variable empty if there are many products}

\item{reporterinreport}{character string leave this variable empty if there are many reporters}

\item{inputpath}{path of the template, defaults to package internal path}

\item{outputdir}{name of the output directory relative to getwd()}

\item{encoding, }{encoding of the template file. See also iconv
The names of encodings and which ones are available are
platform-dependent. All R platforms support ""
(for the encoding of the current locale), "latin1" and "UTF-8".}
}
\description{
This function will generate reports for any template that is
product specific.  Giving information about all world trade
flows for one product.
}
\examples{
\dontrun{
createreport(NULL, template="allproducts.Rmd", reporterinreport ="China")
}
}
