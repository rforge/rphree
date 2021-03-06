% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rphree_Main.R
\name{RPhreeFormatOut}
\alias{RPhreeFormatOut}
\title{Format the output returned by PHREEQC}
\usage{
RPhreeFormatOut(out_list, sel)
}
\arguments{
\item{out_list}{The list formed by the \code{.Call} to PQCLIB}

\item{sel}{The selection of output blocks}
}
\value{
A list (or a list of lists) in which all blocks are
formatted properly in data.frames or named matrices.
}
\description{
This is the workhorse function used for formatting the raw output
returned by PHREEQC at c-level. Not intended for direct use.
}
\details{
TODO
}
\author{
MDL
}
