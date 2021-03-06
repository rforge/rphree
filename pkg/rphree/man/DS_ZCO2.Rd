% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rphree_Solub.R
\name{DS_ZCO2}
\alias{DS_ZCO2}
\title{Compressibility Z of CO2 by the Duan and Sun (2003) model}
\usage{
DS_ZCO2(Tk, Pmpa, verbose = FALSE)
}
\arguments{
\item{Tk}{Temperature in Kelvin}

\item{Pmpa}{Pressure in MPa}

\item{verbose}{Logical. If TRUE, additional messages are showed}
}
\value{
Numeric value of compressibility Z of CO2
}
\description{
Computes the compressibility Z of CO2 by Duan and Sun, 2003.
Internally uses the \code{\link{stats::uniroot}} function.
}
\details{
NONE
}
\author{
MDL
}
