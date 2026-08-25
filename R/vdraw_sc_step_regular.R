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
#' @param atmost1 boolean, report at most 1 event time (alias for `atmostK = 1`)
#' @param atmostK `NULL` or a positive integer: report only the earliest K
#'        event times. Generalizes `atmost1`.
#' @param atleast1 boolean, condition on at least 1 event (alias for `atleastK = 1`)
#' @param atleastK `NULL` or a positive integer: condition on at least K events
#'        in the sampled (sub)interval. Generalizes `atleast1`.
#' @param budget_cap `NULL` or a positive integer: cap the computational event
#'        budget of the kernel. This is an approximation knob (it truncates the
#'        extreme tail of the event-count distribution together with the
#'        `1 - tol` quantile bound), not an exact contract like `atmostK`.
#' @param atmostB deprecated alias for `budget_cap`.
#'
#' @return a vector of event times t
#'         if no events realize, it will have 0 length
#'
#' @examples
#' x <- vdraw_sc_step_regular(
#'   Lambda_matrix = matrix(1:5, nrow = 1),
#'   rate_matrix_t_min = 100,
#'   rate_matrix_t_max = 110,
#'   atmost1 = FALSE
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
    atmostK = NULL,
    atleast1 = FALSE,
    atleastK = NULL,
    budget_cap = NULL,
    atmostB = NULL) {
  atmostK <- .resolve_atmostK(atmost1, atmostK)
  atleastK <- .resolve_atleastK(atleast1, atleastK)
  budget_cap <- .resolve_budget_cap(budget_cap, atmostB)

  if (atleastK >= 1L) {
    return(
      vztdraw_sc_step_regular_cpp(
        lambda_matrix = lambda_matrix,
        Lambda_matrix = Lambda_matrix,
        rate_matrix_t_min = rate_matrix_t_min,
        rate_matrix_t_max = rate_matrix_t_max,
        t_min = t_min,
        t_max = t_max,
        tol = tol,
        atmostK = if (atmostK > 0L) atmostK else NULL,
        atleastK = atleastK,
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
      atmostK = if (atmostK > 0L) atmostK else NULL,
      budget_cap = if (budget_cap > 0L) budget_cap else NULL
    )
  )
}
