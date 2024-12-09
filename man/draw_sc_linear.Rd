% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw_sc_linear.R
\name{draw_sc_linear}
\alias{draw_sc_linear}
\title{Special case: Simulate from a non homogeneous Poisson Point Process (NHPPP) from
(t_min, t_max) with linear intensity function (inversion method)}
\usage{
draw_sc_linear(intercept, slope, t_min, t_max, atmost1 = FALSE)
}
\arguments{
\item{intercept}{(double) the intercept}

\item{slope}{(double) the slope}

\item{t_min}{(double) lower bound of the time interval}

\item{t_max}{(double) upper bound of the time interval}

\item{atmost1}{boolean, draw at most 1 event time}
}
\value{
a vector of event times (t_); if no events realize,
a vector of length 0
}
\description{
Sample NHPPP times from a linear intensity function
using the inversion method, optionally using an \code{rstream}
generator
}
\examples{
x <- draw_sc_linear(intercept = 0, slope = 0.2, t_min = 0, t_max = 10)

}
