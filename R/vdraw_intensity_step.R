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
#' @param lambda_args (list) optional named list of arguments to pass to `lambda`.
#'        If you have arguments for `lambda` that vary by draw, they should be passed as
#'        a data.table named `vector_arguments`.
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
#' @param atmost1 boolean, report at most 1 event time (alias for `atmostK = 1`)
#' @param atmostK `NULL` or a positive integer: report only the earliest K
#'        event times. Generalizes `atmost1`.
#' @param atleast1 boolean, condition on at least 1 accepted event (alias for
#'        `atleastK = 1`)
#' @param atleastK `NULL` or a positive integer: condition on at least K
#'        accepted events (rejection resampling; see
#'        `vztdraw_intensity_step()`). Generalizes `atleast1`.
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
    atmostK = NULL,
    atleast1 = FALSE,
    atleastK = NULL,
    budget_cap = NULL) {
  atmostK <- .resolve_atmostK(atmost1, atmostK)
  atleastK <- .resolve_atleastK(atleast1, atleastK)
  budget_cap <- .resolve_budget_cap(budget_cap, NULL)

  if (atleastK >= 1L) {
    return(vztdraw_intensity_step(
      lambda = lambda,
      lambda_args = lambda_args,
      Lambda_maj_matrix = Lambda_maj_matrix,
      lambda_maj_matrix = lambda_maj_matrix,
      time_breaks = time_breaks,
      t_min = t_min,
      t_max = t_max,
      tol = tol,
      atmostK = if (atmostK > 0L) atmostK else NULL,
      atleastK = atleastK,
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

  if (is.null(lambda_args)) {
    l_ <- lambda
  } else {
    l_ <- function(X, ...) {
      return(lambda(X, lambda_args))
    }
  }

  use_subinterval <- !is.null(args$subinterval)
  subinterval <- if (use_subinterval) args$subinterval else matrix(0, 1, 2)

  return(
    .Call(
      `_nhppp_vdraw_intensity_step_general`, l_,
      args$rate, args$is_cumulative, args$time_breaks, subinterval,
      use_subinterval, tol, atmostK, 0L, budget_cap
    )
  )
}
