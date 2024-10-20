#' Vectorized sampling from a non homogeneous Poisson Point Process (NHPPP) from
#'    an interval (thinning method) with piecewise constant_majorizers (C++)
#'
#' @description
#' Vectorized sampling from a non homogeneous Poisson Point Process (NHPPP) from
#'    an interval (thinning method) with piecewise constant_majorizers.
#'    The majorizers are step functions over equal-length time intevals.
#'
#' @param lambda (function) intensity function, vectorized
#' @param lambda_args (list) optional arguments to pass to `lambda`
#' @param Lambda_maj_matrix (matrix) integrated intensity rates at the end of each interval
#' @param lambda_maj_matrix (matrix) intensity rates, one per interval
#' @param rate_matrix_t_min (scalar | vector | column matrix) is the lower bound
#'        of the time interval for each row of (Lambda|lambda)_maj_matrix.
#'        The length of this argument is the number of point processes that should be drawn.
#' @param rate_matrix_t_max (scalar | vector | column matrix) the upper bound
#'        of the time interval for each row of (Lambda|lambda)_maj_matrix.
#'        The length of this argument is the number of point processes that should be drawn.
#' @param t_min (scalar | vector | column matrix) is the lower bound
#'        of a subinterval of (rate_matrix_t_min, rate_matrix_t_max]. If set,
#'        times are sampled from the subinterval.
#'        If omitted, it is equivalent to `rate_matrix_t_min`.
#' @param t_max (scalar | vector | column matrix) is the upper bound
#'        of a subinterval of (rate_matrix_t_min, rate_matrix_t_max]. If set,
#'        times are sampled from the subinterval.
#'        If omitted, it is equivalent to `rate_matrix_t_max`.
#' @param tol (scalar, double) tolerance for the number of events
#' @param atmost1 boolean, draw at most 1 event time
#' @param atleast1 boolean, draw at least 1 event time
#' @param atmostB If not NULL, draw at most B (B>0) event times. NULL means ignore.
#'
#'
#' @return a matrix of event times (columns) per draw (rows)
#'         NAs are structural empty spots
#'
#' @examples
#' x <- vdraw_intensity(
#'   lambda = function(x, ...) 0.1 * x,
#'   lambda_maj_matrix = matrix(rep(1, 5), nrow = 1),
#'   rate_matrix_t_min = 1,
#'   rate_matrix_t_max = 5
#' )
#' @export


vdraw_intensity <- function(
    lambda = NULL,
    lambda_args = NULL,
    Lambda_maj_matrix = NULL,
    lambda_maj_matrix = NULL,
    rate_matrix_t_min = NULL,
    rate_matrix_t_max = NULL,
    t_min = NULL,
    t_max = NULL,
    tol = 10^-6,
    atmost1 = FALSE,
    atleast1 = FALSE,
    atmostB = NULL) {
  if (isFALSE(atleast1)) {
    return(
      vdraw_intensity_step_regular_cpp(
        lambda = lambda,
        lambda_args = lambda_args,
        Lambda_maj_matrix = Lambda_maj_matrix,
        lambda_maj_matrix = lambda_maj_matrix,
        rate_matrix_t_min = rate_matrix_t_min,
        rate_matrix_t_max = rate_matrix_t_max,
        t_min = t_min,
        t_max = t_max,
        tol = tol,
        atmost1 = atmost1,
        atmostB = atmostB
      )
    )
  }

  if (isTRUE(atleast1)) {
    return(
      vztdraw_intensity_step_regular(
        lambda = lambda,
        lambda_args = lambda_args,
        Lambda_maj_matrix = Lambda_maj_matrix,
        lambda_maj_matrix = lambda_maj_matrix,
        rate_matrix_t_min = rate_matrix_t_min,
        rate_matrix_t_max = rate_matrix_t_max,
        t_min = t_min,
        t_max = t_max,
        tol = tol,
        atmost1 = atmost1
      )
    )
  }
}
