#' Vectorized sampling from NHPPPs with piecewise constant intensities
#' with same interval lengths (R)
#'
#' @description 
#' Simulate a piecewise constant-rate Poisson Point Process over `(t_min, t_max]` (inversion method)
#' where the intervals have the same length (are "regular").
#' This function is internal, and used mainly for testing the C++ version.
#' 
#' It is superseded by the C++ implementation.
#' 
#'
#' @param Lambda_matrix (matrix) integrated intensity rates at the end of each interval
#'        The number of rows is the number of point processes to draw.
#' @param lambda_matrix (matrix) intensity rates, one per interval
#'        The number of rows is the number of point processes to draw.
#' @param rate_matrix_t_min (scalar | vector | column matrix) is the lower bound 
#'        of the time interval for each row of [Lambda|lambda] matrix.
#'        The length of this argument is the number of point processes that should be drawn.
#' @param rate_matrix_t_max (scalar | vector | column matrix) the upper bound 
#'        of the time interval for each row of [Lambda|lambda] matrix.
#'        The length of this argument is the number of point processes that should be drawn.
#' @param tol (scalar, double) tolerance for the number of events
#' @param atmost1 boolean, draw at most 1 event time
#'
#' @return a vector of event times t
#'         if no events realize, it will have 0 length
#'
#' @keywords internal
#' @examples
#' x <- vdraw_sc_step_regular_R(Lambda_matrix = matrix(1:5, nrow = 1))

vdraw_sc_step_regular_R <- function(Lambda_matrix = NULL,
                                    lambda_matrix = NULL,
                                    rate_matrix_t_min = NULL,
                                    rate_matrix_t_max = NULL,
                                    tol = 10^-6,
                                    atmost1 = FALSE) {
  range_t <- cbind(as.vector(rate_matrix_t_min), as.vector(rate_matrix_t_max))
  if (is.null(Lambda_matrix) && !is.null(lambda_matrix)) {
    mode(lambda_matrix) <- "numeric"
    Lambda_matrix <- mat_cumsum_columns(lambda_matrix)
  } else {
    mode(Lambda_matrix) <- "numeric"
  }
  n_draws <- nrow(Lambda_matrix)

  if(nrow(range_t)>1 && nrow(range_t) != n_draws) {
    stop("The (rows of) [Lambda|lambda]_matrix and (length of) [rate_matrix_t_min|rate_matrix_t_max] imply different numbers of point processes to be sampled.")
  }

  n_intervals <- ncol(Lambda_matrix)
  interval_duration <- (range_t[, 2] - range_t[, 1]) / n_intervals

  # pad Lambda_matrix with starting zeros to be able to take duration of each interval
  Lambda_matrix <- cbind(rep(0, n_draws), Lambda_matrix)
  if (atmost1) {
    n_max_events <- 1
  } else {
    n_max_events <- stats::qpois(1 - tol, lambda = max(Lambda_matrix), lower.tail = TRUE)
  }

  Tau <- matrix(
    data = stats::rexp(n = n_draws * n_max_events, rate = 1),
    ncol = n_max_events, nrow = n_draws
  )
  Tau <- mat_cumsum_columns_with_vector_ceiling(Tau, ceil = Lambda_matrix[, n_intervals + 1, drop = FALSE])
  n_max_events <- ncol(Tau)

  IntervalIndex <- matrix(n_intervals + 1, ncol = n_max_events + 1, nrow = n_draws)
  IntervalIndex[, 1] <- 1:n_draws
  AddOneForNextIndex <- matrix(rep(c(0, 1), n_draws), ncol = 2, byrow = TRUE)

  # back transform Tau
  for (ev in 1:n_max_events) {
    for (i in 1:(n_intervals)) {
      is_tau_in_this_interval <- (Lambda_matrix[, i] < Tau[, ev] & Lambda_matrix[, i + 1] >= Tau[, ev])
      IntervalIndex[, ev + 1] <- i * is_tau_in_this_interval + IntervalIndex[, ev + 1] * (!is_tau_in_this_interval)
    }

    Tau[, ev] <- range_t[, 1] +
      ((IntervalIndex[, ev + 1] - 1) + # 0-based indexing of intervals here
        (Tau[, ev] - Lambda_matrix[IntervalIndex[, c(1, ev + 1), drop = FALSE]]) /
          (Lambda_matrix[IntervalIndex[, c(1, ev + 1), drop = FALSE] + AddOneForNextIndex] -
            Lambda_matrix[IntervalIndex[, c(1, ev + 1), drop = FALSE]])
      ) * interval_duration
  }
  return(Tau)
}
