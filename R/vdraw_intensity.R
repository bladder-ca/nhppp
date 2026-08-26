#' Vectorized sampling from a non homogeneous Poisson Point Process (NHPPP) from
#'    an interval (thinning method) with piecewise constant_majorizers (C++)
#'
#' @description
#' Vectorized sampling from a non homogeneous Poisson Point Process (NHPPP) from
#'    an interval (thinning method) with piecewise constant_majorizers.
#'    The majorizers are step functions over equal-length time intevals.
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
#' @param atmost1 boolean, report at most 1 event time (alias for `atmostK = 1`)
#' @param atmostK `NULL` or a positive integer: report only the earliest K
#'        event times. Generalizes `atmost1`.
#' @param atleast1 boolean, condition on at least 1 event (alias for `atleastK = 1`)
#' @param atleastK `NULL` or a positive integer: condition on at least K
#'        accepted events (rejection resampling; see
#'        `vztdraw_intensity_step_regular()`). Generalizes `atleast1`.
#' @param budget_cap `NULL` or a positive integer: cap the computational event
#'        budget of the majorizer kernel (approximation knob).
#' @param atmostB deprecated alias for `budget_cap`.
#'
#'
#' @return a matrix of event times (columns) per draw (rows)
#'         NAs are structural empty spots
#'
#' @examples
#' x <- vdraw_intensity(
#'   lambda = function(x, ...) 0.1 * x,
#'   lambda_maj_matrix = matrix(rep(1, 5), nrow = 1),
#'   rate_matrix_t_min = 1,
#'   rate_matrix_t_max = 5
#' )
#' @export


vdraw_intensity <- function(
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
    atmostK = NULL,
    atleast1 = FALSE,
    atleastK = NULL,
    budget_cap = NULL,
    atmostB = NULL) {
  atmostK <- .resolve_atmostK(atmost1, atmostK)
  atleastK <- .resolve_atleastK(atleast1, atleastK)
  budget_cap <- .resolve_budget_cap(budget_cap, atmostB)

  if (atleastK >= 1L) {
    return(vztdraw_intensity_step_regular(
      lambda = lambda,
      lambda_args = lambda_args,
      Lambda_maj_matrix = Lambda_maj_matrix,
      lambda_maj_matrix = lambda_maj_matrix,
      rate_matrix_t_min = rate_matrix_t_min,
      rate_matrix_t_max = rate_matrix_t_max,
      t_min = t_min,
      t_max = t_max,
      tol = tol,
      atmostK = if (atmostK > 0L) atmostK else NULL,
      atleastK = atleastK
    ))
  }
  return(vdraw_intensity_step_regular_cpp(
    lambda = lambda,
    lambda_args = lambda_args,
    Lambda_maj_matrix = Lambda_maj_matrix,
    lambda_maj_matrix = lambda_maj_matrix,
    rate_matrix_t_min = rate_matrix_t_min,
    rate_matrix_t_max = rate_matrix_t_max,
    t_min = t_min,
    t_max = t_max,
    tol = tol,
    atmostK = if (atmostK > 0L) atmostK else NULL,
    budget_cap = if (budget_cap > 0L) budget_cap else NULL
  ))
}
