#' Vectorized sampling from NHPPPs given the intensity function, with
#' piecewise constant majorizers over arbitrary interval bounds (C++)
#'
#' @description
#' Vectorized thinning sampler: candidate times are proposed from a piecewise
#' constant majorizer and accepted with probability
#' `lambda(t) / lambda_maj(interval of t)`. Unlike the `sc_step` samplers,
#' `lambda` is an arbitrary (vectorized) R function — the step structure only
#' concerns the majorizer. The majorizer intervals need not have the same
#' length: their bounds are given in `time_breaks`, either once for all point
#' processes or per point process. This generalizes
#' `vdraw_intensity()`, which assumes equal-length ("regular") intervals.
#'
#' @param lambda (function) intensity function, vectorized
#' @param lambda_args (list) arguments for `lambda`, with up to two elements:
#'        `shared` (named list of row-invariant arguments of any type, stored
#'        once and never replicated) and `row_args` (data.frame or data.table
#'        with one row per point process, auto-subset when rows are resampled).
#'        When non-`NULL`, the container is passed as `lambda`'s second
#'        positional argument exactly as given, except that `row_args` is
#'        already row-subset; the name of the second formal is up to you. A
#'        flat list with neither element is treated as all-shared; a
#'        `vector_arguments` element is deprecated (use `row_args`).
#' @param Lambda_maj_matrix (matrix) integrated majorizer intensity rates at
#'        the end of each interval
#' @param lambda_maj_matrix (matrix) majorizer intensity rates, one per interval
#' @param time_breaks (vector | matrix) the bounds of the majorizer intervals.
#'        With `K` intervals (`K = ncol([Lambda|lambda]_maj_matrix)`), either a
#'        vector of `K+1` increasing values (the same bounds for all point
#'        processes) or a matrix with `K+1` columns whose i-th row holds the
#'        bounds for the i-th point process (a 1-row matrix is recycled for
#'        all point processes).
#' @param t_min (scalar | vector | column matrix) is the lower bound
#'        of a subinterval of `(time_breaks[, 1], time_breaks[, K+1]]`. If set,
#'        times are sampled from the subinterval.
#' @param t_max (scalar | vector | column matrix) the upper bound
#'        of a subinterval of `(time_breaks[, 1], time_breaks[, K+1]]`. If set,
#'        times are sampled from the subinterval.
#' @param tol (scalar, double) tolerance for the number of events
#' @param atmost1 boolean, report at most 1 event time (alias for
#'        `report_first_K = 1`)
#' @param report_first_K `NULL` or a positive integer: report only the
#'        earliest K accepted event times (reporting truncation — the count
#'        law is unchanged). At most one of `report_first_K`/`report_last_K`
#'        may be set.
#' @param report_last_K `NULL` or a positive integer: report only the latest
#'        K accepted event times (ascending order; reporting truncation).
#' @param atleast1 boolean, condition on at least 1 accepted event (alias for
#'        `generate_at_least_K = 1`)
#' @param generate_at_least_K `NULL` or a positive integer: condition on at
#'        least K accepted events (rejection resampling; see
#'        `vztdraw_intensity_step()`).
#' @param generate_at_most_K `NULL` or a positive integer: condition on at
#'        most K accepted events (rejection resampling on the upper bound).
#' @param budget_cap `NULL` or a positive integer: cap the computational event
#'        budget of the majorizer kernel (approximation knob).
#'
#' @return a matrix of event times (columns) per draw (rows);
#'         NAs are structural empty spots
#'
#' @examples
#' x <- vdraw_intensity_step(
#'   lambda = function(x, ...) 0.1 * x,
#'   lambda_maj_matrix = matrix(rep(1, 5), nrow = 1),
#'   time_breaks = c(1, 1.5, 3, 4, 4.5, 5)
#' )
#' @export
vdraw_intensity_step <- function(
    lambda = NULL,
    lambda_args = NULL,
    Lambda_maj_matrix = NULL,
    lambda_maj_matrix = NULL,
    time_breaks = NULL,
    t_min = NULL,
    t_max = NULL,
    tol = 10^-6,
    atmost1 = FALSE,
    report_first_K = NULL,
    report_last_K = NULL,
    atleast1 = FALSE,
    generate_at_least_K = NULL,
    generate_at_most_K = NULL,
    budget_cap = NULL) {
  rep_ <- .resolve_reporting(atmost1, report_first_K, report_last_K)
  gen_ <- .resolve_generation(atleast1, generate_at_least_K, generate_at_most_K)
  budget_cap <- .resolve_budget_cap(budget_cap, NULL)

  if (gen_$at_least > 0L || gen_$at_most > 0L) {
    return(vztdraw_intensity_step(
      lambda = lambda,
      lambda_args = lambda_args,
      Lambda_maj_matrix = Lambda_maj_matrix,
      lambda_maj_matrix = lambda_maj_matrix,
      time_breaks = time_breaks,
      t_min = t_min,
      t_max = t_max,
      tol = tol,
      report_first_K = if (rep_$first > 0L) rep_$first else NULL,
      report_last_K = if (rep_$last > 0L) rep_$last else NULL,
      generate_at_least_K = if (gen_$at_least > 0L) gen_$at_least else NULL,
      generate_at_most_K = if (gen_$at_most > 0L) gen_$at_most else NULL,
      budget_cap = if (budget_cap > 0L) budget_cap else NULL
    ))
  }

  args <- .prep_vdraw_sc_step_args(
    lambda_matrix = lambda_maj_matrix,
    Lambda_matrix = Lambda_maj_matrix,
    time_breaks = time_breaks,
    t_min = t_min,
    t_max = t_max
  )

  fa_ <- .resolve_fun_args(lambda_args, n_draws = nrow(args$rate), arg_name = "lambda_args")
  l_ <- .wrap_fun(lambda, fa_$container)

  use_subinterval <- !is.null(args$subinterval)
  subinterval <- if (use_subinterval) args$subinterval else matrix(0, 1, 2)

  return(
    .Call(
      `_nhppp_vdraw_intensity_step_general`, l_,
      args$rate, args$is_cumulative, args$time_breaks, subinterval,
      use_subinterval, tol, rep_$first, rep_$last, 0L, budget_cap
    )
  )
}
