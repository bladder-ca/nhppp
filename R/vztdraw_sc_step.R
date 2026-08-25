#' Vectorized sampling from zero-truncated NHPPPs with piecewise constant
#' intensities over arbitrary interval bounds (C++)
#'
#' @description
#' Simulate piecewise constant-rate Poisson Point Processes conditional on
#' observing at least one event, where the intervals need not have the same
#' length. The interval bounds are given in `time_breaks`, either once for all
#' point processes or per point process. This generalizes the zero-truncated
#' regular-grid sampler used by `vdraw_sc_step_regular(atleast1 = TRUE)`.
#'
#' @param lambda_matrix (matrix) intensity rates, one per interval
#' @param Lambda_matrix (matrix) integrated intensity rates at the end of each interval
#' @param time_breaks (vector | matrix) the bounds of the intervals over which
#'        the rates apply. With `K` intervals (`K = ncol([Lambda|lambda]_matrix)`),
#'        either a vector of `K+1` increasing values (the same bounds for all
#'        point processes) or a matrix with `K+1` columns whose i-th row holds
#'        the bounds for the i-th point process (a 1-row matrix is recycled for
#'        all point processes).
#' @param t_min (scalar | vector | column matrix) is the lower bound
#'        of a subinterval of `(time_breaks[, 1], time_breaks[, K+1]]`. If set,
#'        times are sampled from the subinterval.
#'        If omitted, it is equivalent to `time_breaks[, 1]`.
#' @param t_max (scalar | vector | column matrix) the upper bound
#'        of a subinterval of `(time_breaks[, 1], time_breaks[, K+1]]`. If set,
#'        times are sampled from the subinterval.
#'        If omitted, it is equivalent to `time_breaks[, K+1]`.
#' @param tol (scalar, double) tolerance for the number of events
#' @param atmost1 boolean, report at most 1 event time (alias for `atmostK = 1`).
#'        The sample is drawn from the conditioned process and the earliest
#'        event is reported.
#' @param atmostK `NULL` or a positive integer: report only the earliest K
#'        event times of the conditioned process. Generalizes `atmost1`.
#' @param atleastK positive integer: condition on at least K events in the
#'        sampled (sub)interval. `atleastK = 1` (default) is the zero-truncated
#'        process.
#' @param budget_cap `NULL` or a positive integer: cap the computational event
#'        budget of the kernel (approximation knob; never truncates below
#'        `atleastK`).
#'
#' @return a matrix of event times t, with rows corresponding to the sampled point processes.
#'
#' @examples
#' x <- vztdraw_sc_step(
#'   lambda_matrix = matrix(rep(1, 50), nrow = 10),
#'   time_breaks = c(100, 100.5, 102, 106, 109, 110),
#'   atmost1 = FALSE
#' )
#' @export
vztdraw_sc_step <- function(
    lambda_matrix = NULL,
    Lambda_matrix = NULL,
    time_breaks = NULL,
    t_min = NULL,
    t_max = NULL,
    tol = 10^-6,
    atmost1 = FALSE,
    atmostK = NULL,
    atleastK = 1,
    budget_cap = NULL) {
  atmostK <- .resolve_atmostK(atmost1, atmostK)
  atleastK <- .resolve_atleastK(atleast1 = FALSE, atleastK = atleastK)
  if (atleastK < 1L) stop("`atleastK` must be >= 1 for the truncated (zt) samplers")
  budget_cap <- .resolve_budget_cap(budget_cap, NULL)

  args <- .prep_vdraw_sc_step_args(
    lambda_matrix = lambda_matrix,
    Lambda_matrix = Lambda_matrix,
    time_breaks = time_breaks,
    t_min = t_min,
    t_max = t_max
  )

  subinterval <- args$subinterval
  if (is.null(subinterval)) {
    # whole-range sampling: the subinterval is the outer bounds
    subinterval <- args$time_breaks[, c(1, ncol(args$time_breaks)), drop = FALSE]
  }

  return(
    .Call(
      `_nhppp_vztdraw_sc_step_general2`,
      args$rate, args$is_cumulative, args$time_breaks, subinterval,
      tol, atmostK, atleastK, budget_cap
    )
  )
}
