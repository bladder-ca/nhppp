#' Vectorized generic function for simulating from NHPPPs given the intensity function
#' or the cumulative intensity function
#'
#' @description
#' This is a wrapper to the package's specific functions, and thus slightly slower.
#' For time-intensive simulations prefer one of the specific functions.
#'
#' @param lambda (function) intensity function, vectorized
#' @param lambda_args (list) optional arguments to pass to `lambda`
#' @param Lambda_maj_matrix (matrix) integrated intensity rates at the end of each interval
#' @param lambda_maj_matrix (matrix) intensity rates, one per interval
#' @param Lambda (function, double vector) an increasing function
#'        which is the integrated rate of the NHPPP.
#'        It should take a vectorized argument t for times and an optional arguments list.
#' @param Lambda_inv (function, double vector) the inverse of `Lambda()`, also in vectorized form
#'        It should take a vectorized argument z and an optional arguments list.
#' @param Lambda_args (list) optional arguments to pass to Lambda.
#' @param Lambda_inv_args (list) optional arguments to pass to Lambda_inv().
#' @param t_min (scalar | vector | column matrix) is the lower bound
#'        of a subinterval of (rate_matrix_t_min, rate_matrix_t_max]. If set,
#'        times are sampled from the subinterval.
#'        If omitted, it is equivalent to `rate_matrix_t_min`.
#' @param t_max (scalar | vector | column matrix) is the upper bound
#'        of a subinterval of (rate_matrix_t_min, rate_matrix_t_max]. If set,
#'        times are sampled from the subinterval.
#'        If omitted, it is equivalent to `rate_matrix_t_max`.
#' @param rate_matrix_t_min (scalar | vector | column matrix) is the lower bound
#'        of the time interval for each row of (Lambda|lambda)_maj_matrix.
#'        The length of this argument is the number of point processes that should be drawn.
#' @param rate_matrix_t_max (scalar | vector | column matrix) the upper bound
#'        of the time interval for each row of (Lambda|lambda)_maj_matrix.
#'        The length of this argument is the number of point processes that should be drawn.
#' @param tol (scalar, double) tolerance for the number of events
#' @param atmost1 boolean, draw at most 1 event time
#' @param atleast1 boolean, draw at least 1 event time
#' @param atmostB If not NULL, draw at most B (B>0) event times. NULL means ignore.

#'
#' @return a vector of event times
#' @export
vdraw <- function(
    lambda = NULL,
    lambda_args = NULL,
    Lambda_maj_matrix = NULL,
    lambda_maj_matrix = NULL,
    Lambda = NULL,
    Lambda_inv = NULL,
    Lambda_args = NULL,
    Lambda_inv_args = NULL,
    t_min = NULL,
    t_max = NULL,
    rate_matrix_t_min = NULL,
    rate_matrix_t_max = NULL,
    tol = 10^-6,
    atmost1 = FALSE,
    atleast1 = FALSE,
    atmostB = NULL) {
  if (!is.null(lambda) &&
    !(is.null(lambda_maj_matrix) & is.null(lambda_maj_matrix))) {
    return(
      vdraw_intensity(
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
        atleast1 = atleast1,
        atmostB = atmostB
      )
    )
  }

  return(
    vdraw_cumulative_intensity(
      Lambda = Lambda,
      Lambda_inv = Lambda_inv,
      t_min = t_min,
      t_max = t_max,
      Lambda_args = Lambda_args,
      Lambda_inv_args = Lambda_inv_args,
      tol = tol,
      atmost1 = atmost1,
      atleast1 = atleast1
    )
  )
}
