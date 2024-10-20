#' Vectorized sampling from a zero-truncated non homogeneous Poisson Point Process (NHPPP) from
#'    an interval (thinning method) with piecewise constant_majorizers (R)
#'
#' @description
#' Vectorized sampling from a zero-truncated non homogeneous Poisson Point Process (NHPPP) from
#'    an interval (thinning method) with piecewise constant_majorizers.
#'    The majorizers are step functions over equal-length time intevals.
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
#' @param atmost1 boolean, draw at most 1 event time
#' @keywords internal
vztdraw_intensity_step_regular <- function(
    lambda = NULL,
    lambda_args = NULL,
    Lambda_maj_matrix = NULL,
    lambda_maj_matrix = NULL,
    rate_matrix_t_min = NULL,
    rate_matrix_t_max = NULL,
    t_min = NULL,
    t_max = NULL,
    atmost1 = FALSE,
    ...) {
  if (!is.null(lambda_maj_matrix) && is.null(Lambda_maj_matrix)) {
    rate <- lambda_maj_matrix
  } else if (is.null(lambda_maj_matrix) && !is.null(Lambda_maj_matrix)) {
    rate <- Lambda_maj_matrix
  } else {
    stop("lambda_maj_matrix and Lambda_maj_matrix cannot both be `NULL`")
  }

  range_t <- cbind(as.vector(rate_matrix_t_min), as.vector(rate_matrix_t_max))
  if (nrow(range_t) > 1 && nrow(range_t) != nrow(rate)) {
    stop("The (rows of) [Lambda|lambda]_maj_matrix and (length of) [rate_matrix_t_min|rate_matrix_t_max] imply different numbers of point processes to be sampled.")
  }
  if (nrow(range_t) == 1 && nrow(rate) != 1) {
    range_t <- range_t[rep(1, nrow(rate)), ]
  }

  if (is.null(t_min) && is.null(t_max)) {
    subinterval <- range_t
  } else {
    if (is.null(t_min)) t_min <- range_t[, 1, drop = FALSE]
    if (is.null(t_max)) t_max <- range_t[, 2, drop = FALSE]
    subinterval <- cbind(as.vector(t_min), as.vector(t_max))
    if (nrow(subinterval) > 1 && nrow(subinterval) != nrow(rate)) {
      stop("The (rows of) [Lambda|lambda]_maj_matrix and (length of) [t_min|t_max] imply different numbers of point processes to be sampled.")
    }
    if (nrow(subinterval) == 1 && nrow(rate) != 1) {
      subinterval <- subinterval[rep(1, nrow(rate)), ]
    }
    stopifnot(all(subinterval[, 1] >= range_t[, 1]), all(subinterval[, 2] <= range_t[, 2]))
  }

  Z <- vdraw_intensity_step_regular_forcezt(
    lambda = lambda,
    lambda_args = lambda_args,
    Lambda_maj_matrix = Lambda_maj_matrix,
    lambda_maj_matrix = lambda_maj_matrix,
    rate_matrix_t_min = rate_matrix_t_min,
    rate_matrix_t_max = rate_matrix_t_max,
    t_min = t_min,
    t_max = t_max,
    atmost1 = atmost1,
    force_zt_majorizer = TRUE
  )

  has_no_times <- is.na(Z[, 1])
  max_events <- ncol(Z)

  while (sum(has_no_times) > 0) {
    Z_add <- vdraw_intensity_step_regular_forcezt(
      lambda = lambda,
      lambda_args = lambda_args,
      Lambda_maj_matrix = Lambda_maj_matrix[has_no_times, , drop = FALSE],
      lambda_maj_matrix = lambda_maj_matrix[has_no_times, , drop = FALSE],
      rate_matrix_t_min = range_t[has_no_times, 1, drop = FALSE],
      rate_matrix_t_max = range_t[has_no_times, 2, drop = FALSE],
      t_min = subinterval[has_no_times, 1, drop = FALSE],
      t_max = subinterval[has_no_times, 2, drop = FALSE],
      atmost1 = atmost1,
      force_zt_majorizer = TRUE
    ) # force_zt_majorizer)

    diff_cols <- ncol(Z_add) - ncol(Z)
    if (diff_cols > 0) {
      Z <- matrix(
        c(as.vector(Z), rep(NA, times = abs(diff_cols) * nrow(Z))),
        ncol = ncol(Z_add), nrow = nrow(Z), byrow = FALSE
      )
    }
    if (diff_cols < 0) {
      Z_add <- matrix(
        c(as.vector(Z_add), rep(NA, times = abs(diff_cols) * nrow(Z_add))),
        ncol = ncol(Z), nrow = nrow(Z_add), byrow = FALSE
      )
    }

    Z[has_no_times, ] <- Z_add
    has_no_times <- is.na(Z[, 1])
  }
  return(Z)
}
