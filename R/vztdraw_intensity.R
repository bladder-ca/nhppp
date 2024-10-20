#' Vectorized sampling from a zero-truncated non homogeneous Poisson Point Process (NHPPP) from
#'    an interval (thinning method) with piecewise constant_majorizers
#'
#' Vectorized sampling from a zero-truncated non homogeneous Poisson Point Process (NHPPP) from
#'    an interval (thinning method) with piecewise constant_majorizers.
#'    The majorizers are step functions over equal-length time intevals.
#'
#' @param lambda (function) a vectorized intensity function, with one or two arguments.
#'  The first is time. The optional second is a named list with additional arguments.
#' @param lambda_args (list) optional list of named arguments for `lambda()`
#' @param Lambda_maj_matrix (matrix) for the majorizeintegrated intensity rates at the end of each interval
#' @param lambda_maj_matrix (matrix) intensity rates, one per interval
#' @param range_t (vector, or matrix) `t_min` and `t_max`, possibly vectorized
#' @param tol (scalar, double) tolerance for the number of events
#' @param atmost1 boolean, draw at most 1 event time
#' @param ... (any) other arguments (ignored  -- used for flexibility in calling from other functions)
#'
#' @return a matrix of event times (columns) per draw (rows)
#'         NAs are structural empty spots
#' @keywords internal
vztdraw_intensity <- function(
    lambda = NULL,
    lambda_args = NULL,
    Lambda_maj_matrix = NULL,
    lambda_maj_matrix = NULL,
    range_t = NULL,
    tol = 10^-6,
    atmost1 = FALSE,
    ...) {
  return(
    vztdraw_intensity_step_regular(
      lambda = lambda,
      lambda_args = lambda_args,
      Lambda_maj_matrix = Lambda_maj_matrix,
      lambda_maj_matrix = lambda_maj_matrix,
      range_t = range_t,
      tol = tol,
      atmost1 = atmost1,
      ...
    )
  )
}
