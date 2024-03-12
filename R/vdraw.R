#' Vectorized generic function for simulating from NHPPPs given the intensity function
#' or the cumulative intensity function
#'
#' This is a wrapper to the package's specific functions, and thus slightly slower.
#' For time-intensive simulations prefer one of the specific functions.
#'
#' @param lambda (function) a vectorized intensity function, with one or two arguments.
#'  The first is time. The optional second (should be named `lambda_args`) is a named list with additional arguments.
#' @param lambda_args (list) optional list of named arguments for `lambda()`
#' @param Lambda_maj_matrix (matrix) for the majorizeintegrated intensity rates at the end of each interval
#' @param lambda_maj_matrix (matrix) intensity rates, one per interval
#' @param range_t (vector, or matrix) `t_min` and `t_max`, possibly vectorized
#' @param tol (scalar, double) tolerance for the number of events
#' @param atmost1 boolean, draw at most 1 event time
#' @param atleast1 boolean, draw at least 1 event time in interval
#' @param use_cpp boolean, use C++ code
#'
#' @return a vector of event times
#' @examples
#' Z <- vdraw(
#'   lambda = function(x, lambda_args = NULL) 0.1 * x,
#'   range_t = c(1, 10),
#'   lambda_maj_matrix = matrix(rep(1, 5), nrow = 1),
#'   atmost1 = FALSE, atleast1 = FALSE, use_cpp = TRUE
#' )
#' @export
vdraw <- function(
    lambda = NULL,
    lambda_args = NULL,
    Lambda_maj_matrix = NULL,
    lambda_maj_matrix = NULL,
    range_t = NULL,
    tol = 10^-6,
    atmost1 = FALSE,
    atleast1 = FALSE,
    use_cpp = FALSE) {
  if (isTRUE(use_cpp) && isFALSE(atleast1)) {
    return(
      vdraw_intensity_step_regular_cpp(
        lambda = lambda,
        lambda_args = lambda_args,
        Lambda_maj_matrix = Lambda_maj_matrix,
        lambda_maj_matrix = lambda_maj_matrix,
        range_t = range_t,
        tol = tol,
        atmost1 = atmost1
      )
    )
  } else if (isTRUE(use_cpp) && isTRUE(atleast1)) {
    stop("C++ code not implemented yet for this option")
  } else if (isFALSE(use_cpp) && isFALSE(atleast1)) {
    vdraw_intensity_step_regular_R(
      lambda = lambda,
      lambda_args = lambda_args,
      Lambda_maj_matrix = Lambda_maj_matrix,
      lambda_maj_matrix = lambda_maj_matrix,
      range_t = range_t,
      tol = tol,
      atmost1 = atmost1
    )
  } else if (isFALSE(use_cpp) && isTRUE(atleast1)) {
    vztdraw_intensity_step_regular_R(
      lambda = lambda,
      lambda_args = lambda_args,
      Lambda_maj_matrix = Lambda_maj_matrix,
      lambda_maj_matrix = lambda_maj_matrix,
      range_t = range_t,
      tol = tol,
      atmost1 = atmost1
    )
  } else {
    stop("never here")
  }
}
