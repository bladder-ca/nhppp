#' Vectorized sampling from a non homogeneous Poisson Point Process (NHPPP) from
#'    an interval (thinning method) with piecewise constant_majorizers (R)
#'    -- but can be forced to sample from zero-truncated proposals.
#'
#'
#'
#' @description
#' Vectorized sampling from a non homogeneous Poisson Point Process (NHPPP) from
#'    an interval (thinning method) with piecewise constant_majorizers.
#'    The majorizers are step functions over equal-length time intevals.
#'    This function is used for obtainning proposals for `vztdraw_intensity_step_regular()`
#'
#' @param lambda (function) intensity function, vectorized
#' @param lambda_args (list) optional arguments to pass to `lambda`
#' @param Lambda_maj_matrix (matrix) integrated intensity rates at the end of each interval
#' @param lambda_maj_matrix (matrix) intensity rates, one per interval
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
#' @param tol (scalar, double) tolerance for the number of events
#' @param atmost1 boolean, draw at most 1 event time
#' @param force_zt_majorizer boolean, force the majorizer to be zero-truncated.
#'        This option is used when the function is called to make proposals for
#'        `vztdraw_intensity_step_regular()`. In general, do not set this option to `TRUE`.
#'
#' @keywords internal
vdraw_intensity_step_regular_forcezt <- function(
    lambda = NULL,
    lambda_args = NULL,
    Lambda_maj_matrix = NULL,
    lambda_maj_matrix = NULL,
    rate_matrix_t_min = NULL,
    rate_matrix_t_max = NULL,
    t_min = NULL,
    t_max = NULL,
    tol = 10^-6,
    atmost1 = FALSE,
    force_zt_majorizer = FALSE,
    ...) {
  if (!is.null(lambda_maj_matrix) && is.null(Lambda_maj_matrix)) {
    mode(lambda_maj_matrix) <- "numeric"
    rate <- lambda_maj_matrix
    is_cumulative_rate <- FALSE
  } else if (is.null(lambda_maj_matrix) && !is.null(Lambda_maj_matrix)) {
    mode(Lambda_maj_matrix) <- "numeric"
    rate <- Lambda_maj_matrix
    is_cumulative_rate <- TRUE
  } else {
    stop("lambda_maj_matrix and Lambda_maj_matrix cannot both be `NULL`")
  }

  n_intervals <- ncol(rate)
  n_draws <- nrow(rate)


  range_t <- cbind(as.vector(rate_matrix_t_min), as.vector(rate_matrix_t_max))
  if (nrow(range_t) > 1 && nrow(range_t) != nrow(rate)) {
    stop("The (rows of) [Lambda|lambda]_maj_matrix and (length of) [rate_matrix_t_min|rate_matrix_t_max] imply different numbers of point processes to be sampled.")
  }
  if (nrow(range_t) == 1 && nrow(rate) != 1) {
    range_t <- range_t[rep(1, nrow(rate)), ]
  }
  interval_duration <- (range_t[, 2] - range_t[, 1]) / n_intervals

  if (is_cumulative_rate) {
    lambda_maj_matrix <- matrix(0, ncol = n_intervals, nrow = n_draws)
    lambda_maj_matrix[, 1] <- Lambda_maj_matrix[, 1] / interval_duration
    for (col in 2:n_intervals) {
      lambda_maj_matrix[, col] <- (Lambda_maj_matrix[, col] - Lambda_maj_matrix[, col - 1]) / interval_duration
    }
  }

  if (force_zt_majorizer) {
    Z_star <- vztdraw_sc_step_regular_cpp(
      lambda_matrix = lambda_maj_matrix,
      rate_matrix_t_min = rate_matrix_t_min,
      rate_matrix_t_max = rate_matrix_t_max,
      t_min = t_min,
      t_max = t_max,
      atmost1 = FALSE
    )
  } else {
    Z_star <- vdraw_sc_step_regular_cpp(
      lambda_matrix = lambda_maj_matrix,
      rate_matrix_t_min = rate_matrix_t_min,
      rate_matrix_t_max = rate_matrix_t_max,
      t_min = t_min,
      t_max = t_max,
      tol = tol,
      atmost1 = FALSE
    )
  }

  n_max_events <- ncol(Z_star)
  U <- matrix(stats::runif(length(Z_star)), ncol = n_max_events)

  # Extracts correct majorising values
  lambda_maj_idx <- ceiling((Z_star - range_t[, rep(1, n_max_events)]) / interval_duration)

  # browser()
  idx_adjusted <- as.vector(lambda_maj_idx + (1:nrow(lambda_maj_matrix) - 1) * ncol(lambda_maj_matrix))
  lambda_maj <- matrix(
    t(lambda_maj_matrix)[idx_adjusted],
    nrow = nrow(lambda_maj_idx), ncol = ncol(lambda_maj_idx), byrow = FALSE
  )

  accept <- ifelse(lambda(Z_star, lambda_args) / lambda_maj > U, TRUE, NA)
  Z <- Z_star * accept
  Z_sorted <- matrix(
    Z[order(row(Z), is.na(Z), method = "radix")], # shifts non-NAs to the left
    nrow = nrow(Z), ncol = ncol(Z), byrow = TRUE
  )

  if (ncol(Z_sorted) > 1) {
    Z_sorted <- Z_sorted[, colSums(!is.na(Z_sorted)) > 0, drop = FALSE] # removes empty columns after the shift
  }

  if (atmost1) {
    return(Z_sorted[, 1, drop = FALSE])
  } else {
    return(Z_sorted)
  }
}
