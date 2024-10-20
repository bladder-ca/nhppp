#' Vectorized sampling from zero-truncated NHPPPs with piecewise constant intensities
#' with same interval lengths
#'
#' @description
#' Simulate a piecewise constant-rate Poisson Point Process over `(t_min, t_max]` (inversion method)
#' where the intervals have the same length (are "regular").
#'
#' @param Lambda_matrix (matrix) integrated intensity rates at the end of each interval
#' @param lambda_matrix (matrix) intensity rates, one per interval
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
#' @param atmost1 boolean, draw at most 1 event time
#'
#' @return a vector of event times t
#'         if no events realize, it will have 0 length
#' @keywords internal
vztdraw_sc_step_regular_cpp <- function(
    lambda_matrix = NULL,
    Lambda_matrix = NULL,
    rate_matrix_t_min = NULL,
    rate_matrix_t_max = NULL,
    t_min = NULL,
    t_max = NULL,
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
  mode(rate) <- "numeric"

  range_t <- cbind(as.vector(rate_matrix_t_min), as.vector(rate_matrix_t_max))
  if (nrow(range_t) > 1 && nrow(range_t) != nrow(rate)) {
    stop("The (rows of) [Lambda|lambda]_matrix and (length of) [rate_matrix_t_min|rate_matrix_t_max] imply different numbers of point processes to be sampled.")
  }
  if (nrow(range_t) == 1 && nrow(rate) != 1) {
    range_t <- range_t[rep(1, nrow(rate)), ]
  }

  if (is.null(t_min) && is.null(t_max)) {
    return(
      .Call(
        `_nhppp_vztdraw_sc_step_regular`,
        rate, is_cumulative_rate, range_t, atmost1
      )
    )
  }


  # if here, at most one of t_min t_max is null
  if (is.null(t_min)) t_min <- range_t[, 1, drop = FALSE]
  if (is.null(t_max)) t_max <- range_t[, 2, drop = FALSE]

  subinterval <- cbind(as.vector(t_min), as.vector(t_max))
  if (nrow(subinterval) > 1 && nrow(subinterval) != nrow(rate)) {
    stop("The (rows of) [Lambda|lambda]_matrix and (length of) [t_min|t_max] imply different numbers of point processes to be sampled.")
  }
  if (nrow(subinterval) == 1 && nrow(rate) != 1) {
    subinterval <- subinterval[rep(1, nrow(rate)), ]
  }
  stopifnot(all(subinterval[, 1] >= range_t[, 1]), all(subinterval[, 2] <= range_t[, 2]))
  return(
    .Call(
      `_nhppp_vztdraw_sc_step_regular2`,
      rate, is_cumulative_rate, range_t, subinterval, atmost1
    )
  )
}
