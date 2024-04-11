#' Vectorized sampling from zero-truncated NHPPPs with piecewise constant intensities
#' with same interval lengths (C++)
#'
#' Simulate a piecewise constant-rate Poisson Point Process over `(t_min, t_max]` (inversion method)
#' where the intervals have the same length (are "regular").
#'
#' @param Lambda_matrix (matrix) integrated intensity rates at the end of each interval
#' @param lambda_matrix (matrix) intensity rates, one per interval
#' @param range_t (vector, double) `t_min` and `t_max`
#' @param subinterval (vector, double) optional -- the subinterval of `range_t` to sample. If `NULL`, the whole range_t is used.
#' @param atmost1 boolean, draw at most 1 event time
#'
#' @return a vector of event times t
#'         if no events realize, it will have 0 length
#' @export
#'
#' @examples
#' x <- vztdraw_sc_step_regular_cpp(Lambda_matrix = matrix(1:5, nrow = 1))
#' @export
vztdraw_sc_step_regular_cpp <- function(Lambda_matrix = NULL,
                                        lambda_matrix = NULL,
                                        range_t = c(0, 10),
                                        subinterval = NULL,
                                        atmost1 = FALSE) {
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
        `_nhppp_vztdraw_sc_step_regular`,
        rate, is_cumulative_rate, range_t, atmost1
      )
    )
  } else {
    # browser()
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
        `_nhppp_vztdraw_sc_step_regular2`,
        rate, is_cumulative_rate, range_t, subinterval, atmost1
      )
    )
  }
}
