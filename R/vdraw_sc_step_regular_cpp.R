#' Vectorized sampling from NHPPPs with piecewise constant intensities
#' with same interval lengths (C++)
#'
#' Simulate a piecewise constant-rate Poisson Point Process over `(t_min, t_max]` (inversion method)
#' where the intervals have the same length (are "regular").
#'
#' @param Lambda_matrix (matrix) integrated intensity rates at the end of each interval
#' @param lambda_matrix (matrix) intensity rates, one per interval
#' @param range_t (vector, double) `t_min` and `t_max`
#' @param subinterval (matrix, double) subinterval of `range_t` to sample from
#' @param tol (scalar, double) tolerance for the number of events
#' @param atmost1 boolean, draw at most 1 event time
#' @param atmostB If not NULL, draw at most B (B>0) event times. NULL means ignore.
#'
#' @return a vector of event times t
#'         if no events realize, it will have 0 length
#' @export
#'
#' @examples
#' x <- vdraw_sc_step_regular(Lambda_matrix = matrix(1:5, nrow = 1))
#' @export
vdraw_sc_step_regular_cpp <- function(
    lambda_matrix = NULL,
    Lambda_matrix = NULL,
    range_t = NULL,
    subinterval = NULL,
    tol = 10^-6,
    atmost1 = FALSE,
    atmostB = NULL) {
  if (is.null(atmostB)) {
    atmostB <- 0 # has to be <=0 in the C++ argument to be ignored
  }

  if (!is.null(lambda_matrix) && is.null(Lambda_matrix)) {
    rate <- lambda_matrix
    is_cumulative_rate <- FALSE
  } else if (is.null(lambda_matrix) && !is.null(Lambda_matrix)) {
    rate <- Lambda_matrix
    is_cumulative_rate <- TRUE
  } else {
    stop("lambda_matrix and Lambda_matrix cannot both be `NULL`")
  }
  if (!is.matrix(range_t)) {
    range_t <- matrix(rep(range_t, each = nrow(rate)), ncol = 2)
  } else if (nrow(range_t) == 1) {
    range_t <- range_t[rep(1, nrow(rate)), , drop = FALSE]
  } else if (nrow(range_t) != nrow(rate)) {
    stop("`range_t` should have as many rows as `lambda_matrix` or `Lambda_matrix`")
  }

  mode(rate) <- "numeric"

  if (is.null(subinterval)) {
    return(
      .Call(
        `_nhppp_vdraw_sc_step_regular`,
        rate, is_cumulative_rate, range_t, tol, atmost1
      )
    )
  } else {
    if (!is.matrix(subinterval)) {
      subinterval <- matrix(rep(subinterval, each = nrow(rate)), ncol = 2)
    } else if (nrow(subinterval) == 1) {
      subinterval <- subinterval[rep(1, nrow(rate)), , drop = FALSE]
    } else if (nrow(subinterval) != nrow(rate)) {
      stop("`subinterval` should have as many rows as `lambda_matrix` or `Lambda_matrix`")
    }
    stopifnot(all(subinterval[, 1] >= range_t[, 1]), all(subinterval[, 2] <= range_t[, 2]))
    return(
      .Call(
        `_nhppp_vdraw_sc_step_regular2`,
        rate, is_cumulative_rate, range_t, subinterval, tol, atmost1, atmostB
      )
    )
  }
}
