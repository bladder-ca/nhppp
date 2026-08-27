#' Vectorized sampling from NHPPPs with piecewise constant intensities
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
#' @param tol (scalar, double) tolerance for the number of events
#' @param atmost1 boolean, report at most 1 event time (alias for
#'        `report_first_K = 1`)
#' @param report_first_K `NULL` or a positive integer: report only the
#'        earliest K event times (a reporting truncation — the count law of
#'        the sampled process is unchanged). At most one of
#'        `report_first_K`/`report_last_K` may be set.
#' @param report_last_K `NULL` or a positive integer: report only the latest
#'        K event times (ascending order; reporting truncation).
#' @param atleast1 boolean, condition on at least 1 event (alias for
#'        `generate_at_least_K = 1`)
#' @param generate_at_least_K `NULL` or a positive integer: condition the
#'        sampled process on at least K events in the (sub)interval
#'        (order-statistics sampling path).
#' @param generate_at_most_K `NULL` or a positive integer: condition the
#'        sampled process on at most K events. May be combined with
#'        `generate_at_least_K` (K1 <= K2; K1 = K2 conditions on exactly K
#'        events).
#' @param budget_cap `NULL` or a positive integer: cap the computational event
#'        budget of the kernel. This is an approximation knob (it truncates the
#'        extreme tail of the event-count distribution together with the
#'        `1 - tol` quantile bound), not an exact reporting or conditioning
#'        contract.
#' @param atmostB deprecated alias for `budget_cap`.
#'
#' @return a vector of event times t
#'         if no events realize, it will have 0 length
#'
#' @examples
#' x <- vdraw_sc_step_regular(
#'   Lambda_matrix = matrix(1:5, nrow = 1),
#'   rate_matrix_t_min = 100,
#'   rate_matrix_t_max = 110
#' )
#' @export
vdraw_sc_step_regular <- function(
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
    atleast1 = FALSE,
    generate_at_least_K = NULL,
    generate_at_most_K = NULL,
    budget_cap = NULL,
    atmostB = NULL) {
  rep_ <- .resolve_reporting(atmost1, report_first_K, report_last_K)
  gen_ <- .resolve_generation(atleast1, generate_at_least_K, generate_at_most_K)
  budget_cap <- .resolve_budget_cap(budget_cap, atmostB)

  if (gen_$at_least > 0L || gen_$at_most > 0L) {
    return(
      vztdraw_sc_step_regular_cpp(
        lambda_matrix = lambda_matrix,
        Lambda_matrix = Lambda_matrix,
        rate_matrix_t_min = rate_matrix_t_min,
        rate_matrix_t_max = rate_matrix_t_max,
        t_min = t_min,
        t_max = t_max,
        tol = tol,
        report_first_K = if (rep_$first > 0L) rep_$first else NULL,
        report_last_K = if (rep_$last > 0L) rep_$last else NULL,
        generate_at_least_K = if (gen_$at_least > 0L) gen_$at_least else NULL,
        generate_at_most_K = if (gen_$at_most > 0L) gen_$at_most else NULL,
        budget_cap = if (budget_cap > 0L) budget_cap else NULL
      )
    )
  }
  return(
    vdraw_sc_step_regular_cpp(
      lambda_matrix = lambda_matrix,
      Lambda_matrix = Lambda_matrix,
      rate_matrix_t_min = rate_matrix_t_min,
      rate_matrix_t_max = rate_matrix_t_max,
      t_min = t_min,
      t_max = t_max,
      tol = tol,
      report_first_K = if (rep_$first > 0L) rep_$first else NULL,
      report_last_K = if (rep_$last > 0L) rep_$last else NULL,
      budget_cap = if (budget_cap > 0L) budget_cap else NULL
    )
  )
}
