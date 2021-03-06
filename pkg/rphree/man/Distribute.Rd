% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rphree_Utils.R
\name{Distribute}
\alias{Distribute}
\title{Distribute properties in an input buffer.}
\usage{
Distribute(input, prop, values, newname = NULL, first = NULL,
  wholeline = TRUE)
}
\arguments{
\item{input}{The initial input buffer.}

\item{prop}{The property whose values need to be distributed.}

\item{values}{The numerical value(s) to distribute across the SOLUTIONS.}

\item{newname}{Optional name of a property which is not initially
present in the input buffer.}

\item{first}{The \code{first} block which will be passed to
\code{\link{RepSol}} if needed}

\item{wholeline}{logical. if TRUE, comments after the properties (i.e. "as HCO3") get also distributed.}
}
\value{
A new input buffer upon which Rphree can be run.
}
\description{
Function to distribute different values of one property
(concentration, temperature, mineral phase,\dots) properties in an
input buffer.
}
\details{
The number of SOLUTIONs in the input buffer must be 1 or equal to
length(values). If the input buffer contains only one SOLUTION,
and length(values)>1, then the new input gets automatically
replicated length(values) times by RepSol. For KINETICS use
\code{DistributeKin}.
}
\author{
MDL
}
