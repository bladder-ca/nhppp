% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vztdraw_intensity_step_regular.R
\name{vztdraw_intensity_step_regular}
\alias{vztdraw_intensity_step_regular}
\title{Vectorized sampling from a zero-truncated non homogeneous Poisson Point Process (NHPPP) from
an interval (thinning method) with piecewise constant_majorizers (R)}
\usage{
vztdraw_intensity_step_regular(
  lambda = NULL,
  lambda_args = NULL,
  Lambda_maj_matrix = NULL,
  lambda_maj_matrix = NULL,
  rate_matrix_t_min = NULL,
  rate_matrix_t_max = NULL,
  t_min = NULL,
  t_max = NULL,
  atmost1 = FALSE,
  ...
)
}
\arguments{
\item{lambda}{(function) intensity function, vectorized}

\item{lambda_args}{(list) optional arguments to pass to \code{lambda}}

\item{Lambda_maj_matrix}{(matrix) integrated intensity rates at the end of each interval}

\item{lambda_maj_matrix}{(matrix) intensity rates, one per interval}

\item{rate_matrix_t_min}{(scalar | vector | column matrix) is the lower bound
of the time interval for each row of (Lambda|lambda)_maj_matrix.
The length of this argument is the number of point processes that should be drawn.}

\item{rate_matrix_t_max}{(scalar | vector | column matrix) the upper bound
of the time interval for each row of (Lambda|lambda)_maj_matrix.
The length of this argument is the number of point processes that should be drawn.}

\item{t_min}{(scalar | vector | column matrix) is the lower bound
of a subinterval of (rate_matrix_t_min, rate_matrix_t_max]. If set,
times are sampled from the subinterval.
If omitted, it is equivalent to \code{rate_matrix_t_min}.}

\item{t_max}{(scalar | vector | column matrix) is the upper bound
of a subinterval of (rate_matrix_t_min, rate_matrix_t_max]. If set,
times are sampled from the subinterval.
If omitted, it is equivalent to \code{rate_matrix_t_max}.}

\item{atmost1}{boolean, draw at most 1 event time}
}
\description{
Vectorized sampling from a zero-truncated non homogeneous Poisson Point Process (NHPPP) from
an interval (thinning method) with piecewise constant_majorizers.
The majorizers are step functions over equal-length time intevals.
}
\keyword{internal}
