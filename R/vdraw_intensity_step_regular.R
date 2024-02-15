#' Vectorized sampling from a non homogeneous Poisson Point Process (NHPPP) from
#'    an interval (thinning method) with piecewise constant_majorizers (R)
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
#' @param tol (scalar, double) tolerance for the number of events
#' @param atmost1 boolean, draw at most 1 event time
#'
#' @return a matrix of event times (columns) per draw (rows)
#'         NAs are structural empty spots
#' @export
#'
#' @examples
#' Z <- vdraw_intensity_step_regular(
#'  lambda = function(x, ...) 0.1 * x,
#'  lambda_maj_matrix = matrix(rep(1, 5), nrow = 1)
#'  )
#' @export
vdraw_intensity_step_regular <- function(lambda,
                                         lambda_args = NULL,
                                         Lambda_maj_matrix = NULL,
                                         lambda_maj_matrix = NULL,
                                         range_t = c(0, 10),
                                         tol = 10^-6,
                                         atmost1 = FALSE,
                                         force_zt = FALSE) {
  #browser()
  if (!is.null(Lambda_maj_matrix)) {
    mode(Lambda_maj_matrix) <- "numeric"
    n_intervals <- ncol(Lambda_maj_matrix)
    n_draws <- nrow(Lambda_maj_matrix)
  } else {
    n_intervals <- ncol(lambda_maj_matrix)
    n_draws <- nrow(lambda_maj_matrix)
  }
  if (!is.matrix(range_t)) {
    range_t <- matrix(range_t, nrow = 1)
  }
  interval_duration <- (range_t[, 2] - range_t[, 1]) / n_intervals

  # Towards the end, I need lambda_maj_matrix
  # make this a helper function
  if (is.null(lambda_maj_matrix)) {
    lambda_maj_matrix <- matrix(0, ncol = n_intervals, nrow = n_draws)
    lambda_maj_matrix[, 1] <- Lambda_maj_matrix[, 1] / interval_duration
    for (col in 2:n_intervals) {
      lambda_maj_matrix[, col] <- (Lambda_maj_matrix[, col] - Lambda_maj_matrix[, col - 1]) / interval_duration
    }
  }
  mode(lambda_maj_matrix) <- "numeric"

  if (!force_zt) {
    vdraw_fun <- vdraw_sc_step_regular
  } else {
    vdraw_fun <- vztdraw_sc_step_regular
  }

  Z_star <- vdraw_fun(
    Lambda_matrix = Lambda_maj_matrix,
    lambda_matrix = lambda_maj_matrix,
    range_t = range_t,
    tol = tol,
    atmost1 = FALSE
  )

  n_max_events <- ncol(Z_star)
  U <- matrix(stats::runif(length(Z_star)), ncol = n_max_events)
  lambda_maj_indices_for_events <- ceiling((Z_star - range_t[, rep(1, n_max_events)]) / interval_duration)

  Z <- matrix(NA, ncol = n_max_events, nrow = n_draws)
  if (atmost1) {
    for (r in 1:n_draws) {
      tmp <- lambda(Z_star[r, ], lambda_args) / lambda_maj_matrix[r, lambda_maj_indices_for_events[r, ]] > U[r, ]
      n_accepted_in_row <- sum(tmp, na.rm = TRUE)
      if (n_accepted_in_row != 0) {
        Z[r, 1:n_accepted_in_row] <- min(Z_star[r, tmp], na.rm = TRUE)
      }
    }
    Z[which(is.infinite(Z[, 1])), 1] <- NA
    return(Z[, 1, drop = FALSE])
  }
  for (r in 1:n_draws) {
    tmp <- lambda(Z_star[r, ], lambda_args) / lambda_maj_matrix[r, lambda_maj_indices_for_events[r, ]] > U[r, ]
    n_accepted_in_row <- sum(tmp, na.rm = TRUE)
    if (n_accepted_in_row != 0) {
      Z[r, 1:n_accepted_in_row] <- sort(Z_star[r, tmp], na.last = TRUE)[1:n_accepted_in_row]
    }
  }
  num_draws_with_na <- colSums(is.na(Z))
  return(Z[,num_draws_with_na != n_draws, drop = FALSE])
}
