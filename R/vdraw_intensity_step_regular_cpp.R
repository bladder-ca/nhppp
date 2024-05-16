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
#' Z <- vdraw_intensity_step_regular_cpp(
#'   lambda = function(x, lambda_args = NULL) 0.1 * x,
#'   range_t = c(1, 10),
#'   lambda_maj_matrix = matrix(rep(1, 5), nrow = 1)
#' )
#' @export
vdraw_intensity_step_regular_cpp <- function(lambda = NULL,
                                             lambda_args = NULL,
                                             Lambda_maj_matrix = NULL,
                                             lambda_maj_matrix = NULL,
                                             range_t = NULL,
                                             subinterval = NULL,
                                             tol = 10^-6,
                                             atmost1 = FALSE,
                                             atmostB = NULL) {
  # browser()
  if (!is.null(lambda_maj_matrix) && is.null(Lambda_maj_matrix)) {
    rate <- lambda_maj_matrix
    is_cumulative_rate <- FALSE
  } else if (is.null(lambda_maj_matrix) && !is.null(Lambda_maj_matrix)) {
    rate <- Lambda_maj_matrix
    is_cumulative_rate <- TRUE
  } else {
    stop("lambda_maj_matrix and Lambda_maj_matrix cannot both be `NULL`")
  }
  if (!is.matrix(range_t)) {
    range_t <- matrix(rep(range_t, each = nrow(rate)), ncol = 2)
  } else if (nrow(range_t) == 1) {
    range_t <- range_t[rep(1, nrow(rate)), , drop = FALSE]
  } else if (nrow(range_t) != nrow(rate)) {
    stop("`range_t` should have as many rows as `lambda_maj_matrix` or `Lambda_maj_matrix`")
  } else {
  }

  if (is.null(atmostB)) {
    atmostB <- -1 # has to be <=0 to be ignored
  }

  mode(rate) <- "numeric"
  if (is.null(lambda_args)) {
    l_ <- lambda
  } else {
    l_ <- function(X, ...) {
      return(lambda(X, lambda_args))
    }
  }

  use_subinterval <- TRUE
  if (is.null(subinterval)) {
    use_subinterval <- FALSE
    subinterval <- range_t
  } else {
    if (!is.matrix(subinterval)) {
      subinterval <- matrix(rep(subinterval, each = nrow(rate)), ncol = 2)
    } else if (nrow(subinterval) == 1) {
      subinterval <- subinterval[rep(1, nrow(rate)), , drop = FALSE]
    } else if (nrow(subinterval) != nrow(rate)) {
      stop("`subinterval` should have as many rows as `lambda_matrix` or `Lambda_matrix`")
    }
    stopifnot(all(subinterval[, 1] >= range_t[, 1]), all(subinterval[, 2] <= range_t[, 2]))
  }

  # browser()

  return(
    .Call(
      `_nhppp_vdraw_intensity_step_regular`, l_,
      rate, is_cumulative_rate, range_t, subinterval, use_subinterval, tol, atmost1, atmostB
    )
  )
}
