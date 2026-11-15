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
#' @param output (string) `"matrix"` (default) returns the NA-padded event
#'        matrix, one row per point process. `"long"` returns the long event
#'        format `list(id, time, n_draws)` with one entry per event: `id` is
#'        the 1-based point-process index (ascending; times ascending within
#'        `id`). A point process with no events contributes no entries, so
#'        its id is absent; `n_draws` distinguishes "no events" from "not
#'        sampled". No `NA` is used. The long format is built without
#'        allocating the dense matrix, so prefer it when event counts vary
#'        widely across point processes.
#' @return a matrix of event times t, with rows corresponding to the sampled
#'        point processes, or the long event format if `output = "long"`.
#'
#' @examples
#' x <- vztdraw_sc_step(
#'   lambda_matrix = matrix(rep(1, 50), nrow = 10),
#'   time_breaks = c(100, 100.5, 102, 106, 109, 110)
#' )
#'
#' # equivalently, via the general conditioning option
#' x <- vdraw_sc_step(
#'   lambda_matrix = matrix(rep(1, 50), nrow = 10),
#'   time_breaks = c(100, 100.5, 102, 106, 109, 110),
#'   generate_at_least_K = 1
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
    report_first_K = NULL,
    report_last_K = NULL,
    generate_at_least_K = 1,
    generate_at_most_K = NULL,
    budget_cap = NULL,
    output = c("matrix", "long")) {
  rep_ <- .resolve_reporting(atmost1, report_first_K, report_last_K)
  gen_ <- .resolve_generation(FALSE, generate_at_least_K, generate_at_most_K)
  if (gen_$at_least == 0L && gen_$at_most == 0L) {
    stop("at least one of `generate_at_least_K`/`generate_at_most_K` must be set for the conditioned (zt) samplers")
  }
  budget_cap <- .resolve_budget_cap(budget_cap, NULL)
  long_ <- .resolve_output(output)

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
      tol, rep_$first, rep_$last, gen_$at_least, gen_$at_most, budget_cap, long_
    )
  )
}
