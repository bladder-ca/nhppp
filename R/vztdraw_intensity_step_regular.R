#' Vectorized sampling from a K-truncated non homogeneous Poisson Point Process (NHPPP) from
#'    an interval (thinning method) with piecewise constant majorizers (R)
#'
#' @description
#' Vectorized thinning sampler conditional on the accepted event count lying
#' in `[generate_at_least_K, generate_at_most_K]` (`generate_at_least_K = 1`
#' alone, the default, is the zero-truncated process).
#' The majorizers are step functions over equal-length time intervals.
#'
#' Conditioning is by rejection: candidate realizations are proposed from the
#' majorizer conditioned on at least K1 majorizer events (only the lower
#' bound may be pushed into the proposal — at least K1 accepted events
#' implies at least K1 majorizer events, whereas an upper bound on the
#' majorizer count would over-restrict and bias the accepted law), thinned
#' against `lambda`, and rows whose surviving count falls outside
#' `[K1, K2]` are resampled until the condition holds. The per-round
#' acceptance probability degrades in the strictness of the bounds and in
#' the looseness of the majorizer, so a tight majorizer matters much more
#' here than in the unconditional sampler. There is no iteration cap.
#'
#' @param lambda (function) intensity function, vectorized
#' @param lambda_args (list) optional named list of arguments to pass to `lambda`.
#'        If you have arguments for `lambda` that vary by draw, they should be passed as
#'        a data.table named `vector_arguments`.
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
#' @param atmost1 boolean, report at most 1 event time of the conditioned
#'        process (alias for `report_first_K = 1`)
#' @param report_first_K `NULL` or a positive integer: report only the
#'        earliest K event times of the conditioned realization.
#' @param report_last_K `NULL` or a positive integer: report only the latest
#'        K event times of the conditioned realization (ascending order).
#' @param generate_at_least_K non-negative integer: condition on at least K
#'        accepted events. The default 1 is the zero-truncated process.
#' @param generate_at_most_K `NULL` or a positive integer: condition on at
#'        most K accepted events (rejection on the upper bound).
#' @param ... absorbed (for wrapper compatibility)
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
    tol = 10^-6,
    atmost1 = FALSE,
    report_first_K = NULL,
    report_last_K = NULL,
    generate_at_least_K = 1,
    generate_at_most_K = NULL,
    ...) {
  rep_ <- .resolve_reporting(atmost1, report_first_K, report_last_K)
  gen_ <- .resolve_generation(FALSE, generate_at_least_K, generate_at_most_K)
  if (gen_$at_least == 0L && gen_$at_most == 0L) {
    stop("at least one of `generate_at_least_K`/`generate_at_most_K` must be set for the conditioned (zt) samplers")
  }

  if (!is.null(lambda_maj_matrix) && is.null(Lambda_maj_matrix)) {
    n_draws <- nrow(lambda_maj_matrix)
  } else if (is.null(lambda_maj_matrix) && !is.null(Lambda_maj_matrix)) {
    n_draws <- nrow(Lambda_maj_matrix)
  } else {
    stop("lambda_maj_matrix and Lambda_maj_matrix cannot both be `NULL`")
  }

  range_t <- cbind(as.vector(rate_matrix_t_min), as.vector(rate_matrix_t_max))
  if (nrow(range_t) == 1 && n_draws != 1) {
    range_t <- range_t[rep(1, n_draws), ]
  }
  if (is.null(t_min)) t_min <- range_t[, 1, drop = FALSE]
  if (is.null(t_max)) t_max <- range_t[, 2, drop = FALSE]
  t_min <- if (length(t_min) == 1) rep(t_min, n_draws) else as.vector(t_min)
  t_max <- if (length(t_max) == 1) rep(t_max, n_draws) else as.vector(t_max)

  has_vector_args <- !is.null(lambda_args$vector_arguments)
  if (has_vector_args) {
    stopifnot(data.table::is.data.table(lambda_args$vector_arguments))
    original_vector_arguments <- lambda_args$vector_arguments
  }

  draw_round <- function(rows) {
    if (has_vector_args) {
      lambda_args$vector_arguments <- original_vector_arguments[rows, , drop = FALSE]
    }
    vdraw_intensity_step_regular_cpp(
      lambda = lambda,
      lambda_args = lambda_args,
      Lambda_maj_matrix = if (!is.null(Lambda_maj_matrix)) Lambda_maj_matrix[rows, , drop = FALSE] else NULL,
      lambda_maj_matrix = if (!is.null(lambda_maj_matrix)) lambda_maj_matrix[rows, , drop = FALSE] else NULL,
      rate_matrix_t_min = range_t[rows, 1, drop = FALSE],
      rate_matrix_t_max = range_t[rows, 2, drop = FALSE],
      t_min = t_min[rows],
      t_max = t_max[rows],
      tol = tol,
      generate_at_least_K = if (gen_$at_least > 0L) gen_$at_least else NULL
    )
  }

  fails_condition <- function(counts) {
    (counts < gen_$at_least) | (gen_$at_most > 0L & counts > gen_$at_most)
  }

  Z <- draw_round(seq_len(n_draws))
  needs_redraw <- fails_condition(rowSums(!is.na(Z)))

  while (any(needs_redraw)) {
    Z_add <- draw_round(which(needs_redraw))

    diff_cols <- ncol(Z_add) - ncol(Z)
    if (diff_cols > 0) {
      Z <- cbind(Z, matrix(NA_real_, nrow = nrow(Z), ncol = diff_cols))
    }
    if (diff_cols < 0) {
      Z_add <- cbind(Z_add, matrix(NA_real_, nrow = nrow(Z_add), ncol = -diff_cols))
    }

    Z[needs_redraw, ] <- Z_add
    needs_redraw <- fails_condition(rowSums(!is.na(Z)))
  }

  return(.report_slice(Z, rep_))
}
