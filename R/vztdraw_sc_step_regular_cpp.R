#' Vectorized sampling from zero-truncated NHPPPs with piecewise constant intensities
#' with same interval lengths
#'
#' @description
#' Simulate a piecewise constant-rate Poisson Point Process over `(t_min, t_max]` (inversion method)
#' where the intervals have the same length (are "regular"), conditional on
#' the event count lying in `[generate_at_least_K, generate_at_most_K]`
#' (`generate_at_least_K = 1` alone, the default, is the zero-truncated
#' process).
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
#' @param tol (scalar, double) tolerance for the number of events
#' @param atmost1 boolean, report at most 1 event time of the conditioned
#'        process (alias for `report_first_K = 1`)
#' @param report_first_K `NULL` or a positive integer: report only the
#'        earliest K event times of the conditioned realization (never
#'        affects the conditioned count itself). At most one of
#'        `report_first_K`/`report_last_K` may be set.
#' @param report_last_K `NULL` or a positive integer: report only the latest
#'        K event times of the conditioned realization (ascending order).
#' @param generate_at_least_K non-negative integer: condition on at least K
#'        events in the sampled (sub)interval. The default 1 is the
#'        zero-truncated process.
#' @param generate_at_most_K `NULL` or a positive integer: condition on at
#'        most K events. May be combined with `generate_at_least_K`
#'        (K1 <= K2; K1 = K2 conditions on exactly K events).
#' @param budget_cap `NULL` or a positive integer: cap the computational event
#'        budget of the kernel (approximation knob; never truncates below
#'        `generate_at_least_K`).
#'
#' @return a matrix of event times t, with rows corresponding to the sampled point processes.
#' @keywords internal
vztdraw_sc_step_regular_cpp <- function(
    lambda_matrix = NULL,
    Lambda_matrix = NULL,
    rate_matrix_t_min = NULL,
    rate_matrix_t_max = NULL,
    t_min = NULL,
    t_max = NULL,
    tol = 10^-6,
    atmost1 = FALSE,
    report_first_K = NULL,
    report_last_K = NULL,
    generate_at_least_K = 1,
    generate_at_most_K = NULL,
    budget_cap = NULL) {
  rep_ <- .resolve_reporting(atmost1, report_first_K, report_last_K)
  gen_ <- .resolve_generation(FALSE, generate_at_least_K, generate_at_most_K)
  if (gen_$at_least == 0L && gen_$at_most == 0L) {
    stop("at least one of `generate_at_least_K`/`generate_at_most_K` must be set for the conditioned (zt) samplers")
  }
  budget_cap <- .resolve_budget_cap(budget_cap, NULL)

  if (!is.null(lambda_matrix) && is.null(Lambda_matrix)) {
    rate <- lambda_matrix
    is_cumulative_rate <- FALSE
  } else if (is.null(lambda_matrix) && !is.null(Lambda_matrix)) {
    rate <- Lambda_matrix
    is_cumulative_rate <- TRUE
  } else {
    stop("lambda_matrix and Lambda_matrix cannot both be `NULL`")
  }
  if (!is.double(rate)) storage.mode(rate) <- "double"
  num_na <- sum(is.na(rate))
  if (num_na > 0) {
    rate_argument <- if (is_cumulative_rate) "Lambda_matrix" else "lambda_matrix"
    stop("The ", rate_argument, " contains ", num_na, " NA values")
  }

  # 1-row matrices are shared across all point processes (the C++ kernel
  # selects row 0), so they are not replicated here
  range_t <- cbind(as.vector(rate_matrix_t_min), as.vector(rate_matrix_t_max))
  if (nrow(range_t) > 1 && nrow(range_t) != nrow(rate)) {
    stop("The (rows of) [Lambda|lambda]_matrix and (length of) [rate_matrix_t_min|rate_matrix_t_max] imply different numbers of point processes to be sampled.")
  }

  if (is.null(t_min) && is.null(t_max)) {
    subinterval <- range_t
  } else {
    # if here, at most one of t_min t_max is null
    if (is.null(t_min)) t_min <- range_t[, 1, drop = FALSE]
    if (is.null(t_max)) t_max <- range_t[, 2, drop = FALSE]

    subinterval <- cbind(as.vector(t_min), as.vector(t_max))
    if (nrow(subinterval) > 1 && nrow(subinterval) != nrow(rate)) {
      stop("The (rows of) [Lambda|lambda]_matrix and (length of) [t_min|t_max] imply different numbers of point processes to be sampled.")
    }
    stopifnot(all(subinterval[, 1] >= range_t[, 1]), all(subinterval[, 2] <= range_t[, 2]))
  }

  return(
    .Call(
      `_nhppp_vztdraw_sc_step_regular2`,
      rate, is_cumulative_rate, range_t, subinterval, tol,
      rep_$first, rep_$last, gen_$at_least, gen_$at_most, budget_cap
    )
  )
}
