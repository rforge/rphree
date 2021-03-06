% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rphree_Solub.R
\name{DS_PhiCO2}
\alias{DS_PhiCO2}
\title{Fugacity coefficient of CO2}
\usage{
DS_PhiCO2(Tk, Pmpa)
}
\arguments{
\item{Tk}{Temperature in Kelvin}

\item{Pmpa}{Pressure in MPa}
}
\value{
Numeric value of the fugacity coefficient of CO2. The
molar volume is also computed and given as \emph{attribute} (if
\code{out} is the output, it becomes accessible with
\code{attr(out,"vol")})
}
\description{
Computes the fugacity coefficient of CO2 by Duan and Sun 2003
model.
}
\details{
TODO
}
\author{
MDL
}
