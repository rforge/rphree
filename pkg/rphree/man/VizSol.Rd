% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Rphree_Viz.R
\name{VizSol}
\alias{VizSol}
\title{Visualize barplot of total solute concentrations of
solution(s)}
\usage{
VizSol(sol, names, vlim, sel, mult, leg = TRUE, flex = TRUE, ...)
}
\arguments{
\item{sol}{A list of solution (output of Rphree). Must be in the
form \code{list(sol1,sol2)}}

\item{names}{Char vector containing the names of the solutions to
visualize e.g., in the legend}

\item{vlim}{inferior bound for visualizations (as in ylim)}

\item{sel}{Char vector containing the names of the elements to
plot}

\item{mult}{named numeric vector containing multiplicative factors
for the elements who need to be magnified in the plot}

\item{leg}{Logical. If TRUE, a legend is plotted}

\item{flex}{Logical. If TRUE, ListInfo is not required and a
specific heuristic ist applied instead}

\item{...}{Further parameters passed to
\code{\link{graphics::barplot}}}
}
\value{
invisible return of \code{\link{graphics::barplot}}
}
\description{
Utility function to visualize and compare the concentrations of
total soluted elements using barplot.
}
\author{
MDL
}
