#' Vectorized sampling from NHPPPs with piecewise constant intensities
#' with same interval lengths
#'
#' Simulate a piecewise constant-rate Poisson Point Process over `(t_min, t_max]` (inversion method)
#' where the intervals have the same length (are "regular").
#'
#' @param Lambda_matrix (matrix) integrated intensity rates at the end of each interval
#' @param lambda_matrix (matrix) intensity rates, one per interval
#' @param range_t (vector, double) `t_min` and `t_max`
#' @param tol (scalar, double) tolerance for the number of events
#' @param atmost1 boolean, draw at most 1 event time
#'
#' @return a vector of event times t
#'         if no events realize, it will have 0 length
#' @export
#'
#' @examples
#' x <- vdraw_sc_step_regular(Lambda_matrix = matrix(1:5, nrow = 1))
#' @export
vdraw_sc_step_regular <- function(Lambda_matrix = NULL,
                                  lambda_matrix = NULL,
                                  range_t = c(0, 10),
                                  tol = 10^-6,
                                  atmost1 = FALSE) {
  # browser()

  if (is.null(Lambda_matrix) && !is.null(lambda_matrix)) {
    Lambda_matrix <- mat_cumsum_columns(lambda_matrix)
  }
  n_draws <- nrow(Lambda_matrix)
  n_intervals <- ncol(Lambda_matrix)
  interval_duration <- (range_t[2] - range_t[1]) / n_intervals

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
  Tau <- mat_cumsum_columns_with_vector_ceiling(Tau, ceil = Lambda_matrix[, n_intervals + 1])
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

    Tau[, ev] <- range_t[1] +
      ((IntervalIndex[, ev + 1] - 1) + # 0-based indexing of intervals here
        (Tau[, ev] - Lambda_matrix[IntervalIndex[, c(1, ev + 1), drop = FALSE]]) /
          (Lambda_matrix[IntervalIndex[, c(1, ev + 1), drop = FALSE] + AddOneForNextIndex] -
            Lambda_matrix[IntervalIndex[, c(1, ev + 1), drop = FALSE]])
      ) * interval_duration
  }
  return(Tau)
}
