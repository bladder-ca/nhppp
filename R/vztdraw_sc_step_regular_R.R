#' Vectorized sampling from zero-truncated NHPPPs with piecewise constant intensities
#' with same interval lengths (R)
#'
#' Simulate a piecewise constant-rate Poisson Point Process over `(t_min, t_max]` (inversion method)
#' where the intervals have the same length (are "regular").
#'
#' @param Lambda_matrix (matrix) integrated intensity rates at the end of each interval
#' @param lambda_matrix (matrix) intensity rates, one per interval
#' @param range_t (vector, double) `t_min` and `t_max`
#' @param atmost1 boolean, draw at most 1 event time
#' @param ... (any) other arguments (ignored  -- used for flexibility in calling from other functions)
#'
#' @return a vector of event times t
#'         if no events realize, it will have 0 length
#' @export
#'
#' @examples
#' x <- vztdraw_sc_step_regular_R(Lambda_matrix = matrix(1:5, nrow = 1))
#' @export
vztdraw_sc_step_regular_R <- function(Lambda_matrix = NULL,
                                      lambda_matrix = NULL,
                                      range_t = c(0, 10),
                                      atmost1 = FALSE,
                                      ...) {
  # browser()
  if (!is.matrix(range_t)) {
    range_t <- matrix(range_t, nrow = 1)
  }
  if (is.null(Lambda_matrix) && !is.null(lambda_matrix)) {
    mode(lambda_matrix) <- "numeric"
    Lambda_matrix <- mat_cumsum_columns(lambda_matrix)
  } else {
    mode(Lambda_matrix) <- "numeric"
  }
  n_draws <- nrow(Lambda_matrix)
  n_intervals <- ncol(Lambda_matrix)
  interval_duration <- (range_t[, 2] - range_t[, 1]) / n_intervals

  # pad Lambda_matrix with starting zeros to be able to take duration of each interval
  Lambda_matrix <- cbind(rep(0, n_draws), Lambda_matrix)

  n_events <- rztpois(size = n_draws, lambda = Lambda_matrix[, n_intervals + 1])


  if (atmost1) {
    n_max_events <- 1
    Tau <- matrix(rep(NA, n_draws), ncol = 1)
  } else {
    n_max_events <- max(n_events)
    Tau <- matrix(NA, ncol = n_max_events, nrow = n_draws)
  }
  # This part can be sped up in C
  # n_events_cumsum <- cumsum(n_events)
  # U <- stats::runif(n_events_cumsum[n_draws])
  for (r in 1:n_draws) {
    if (atmost1) {
      Tau[r, 1] <- min(stats::runif(n_events[r])) * Lambda_matrix[r, n_intervals + 1]
    } else {
      Tau[r, 1:(n_events[r])] <- sort(stats::runif(n_events[r])) * Lambda_matrix[r, n_intervals + 1]
    }
  }


  IntervalIndex <- matrix(n_intervals + 1, ncol = n_max_events + 1, nrow = n_draws)
  IntervalIndex[, 1] <- 1:n_draws
  AddOneForNextIndex <- matrix(rep(c(0, 1), n_draws), ncol = 2, byrow = TRUE)

  # browser()
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
