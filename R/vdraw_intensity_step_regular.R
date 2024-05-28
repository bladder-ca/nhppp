#' Vectorized sampling from a non homogeneous Poisson Point Process (NHPPP) from
#'    an interval (thinning method) with piecewise constant_majorizers (C++)
#'
#' Vectorized sampling from a non homogeneous Poisson Point Process (NHPPP) from
#'    an interval (thinning method) with piecewise constant_majorizers.
#'    The majorizers are step functions over equal-length time intevals.
#'
#' @param lambda (function) a vectorized intensity function, with one or two arguments.
#'  The first is time. The optional second is a named list with additional arguments.
#' @param lambda_args (list) optional list of named arguments for `lambda()`
#' @param Lambda_maj_matrix (matrix) for the majorizeintegrated intensity rates at the end of each interval
#' @param lambda_maj_matrix (matrix) intensity rates, one per interval
#' @param range_t (vector, or matrix) `t_min` and `t_max`, possibly vectorized
#' @param subinterval (matrix, double) subinterval of `range_t` to sample from
#' @param tol (scalar, double) tolerance for the number of events
#' @param atmost1 boolean, draw at most 1 event time
#' @param atmostB If not NULL, draw at most B (B>0) event times. NULL means ignore.
#'
#' @return a matrix of event times (columns) per draw (rows)
#'         NAs are structural empty spots
#' @export
#'
#' @examples
#' Z <- vdraw_intensity_step_regular(
#'   lambda = function(x, lambda_args = NULL) 0.1 * x,
#'   range_t = c(1, 10),
#'   lambda_maj_matrix = matrix(rep(1, 5), nrow = 1)
#' )
#' @export


vdraw_intensity_step_regular <- function(lambda = NULL,
                                         lambda_args = NULL,
                                         Lambda_maj_matrix = NULL,
                                         lambda_maj_matrix = NULL,
                                         range_t = NULL,
                                         subinterval = NULL,
                                         tol = 10^-6,
                                         atmost1 = FALSE,
                                         atmostB = NULL) {
  return(
    vdraw_intensity_step_regular_cpp(
      lambda = lambda,
      lambda_args = lambda_args,
      Lambda_maj_matrix = Lambda_maj_matrix,
      lambda_maj_matrix = lambda_maj_matrix,
      range_t = range_t,
      subinterval = subinterval,
      tol = tol,
      atmost1 = atmost1,
      atmostB = atmostB
    )
  )
}
