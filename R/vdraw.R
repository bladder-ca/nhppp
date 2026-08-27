#' Vectorized generic function for simulating from NHPPPs given the intensity function
#' or the cumulative intensity function
#'
#' @description
#' This is a wrapper to the package's specific functions, and thus slightly slower.
#' For time-intensive simulations prefer one of the specific functions.
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
#' @param Lambda_maj_matrix (matrix) integrated intensity rates at the end of each interval
#' @param lambda_maj_matrix (matrix) intensity rates, one per interval
#' @param Lambda (function, double vector) an increasing function
#'        which is the integrated rate of the NHPPP.
#'        It should take a vectorized argument t for times and an optional arguments list.
#' @param Lambda_inv (function, double vector) the inverse of `Lambda()`, also in vectorized form
#'        It should take a vectorized argument z and an optional arguments list.
#' @param Lambda_args (list) arguments for BOTH `Lambda` and `Lambda_inv`, with
#'        up to two elements: `shared` (named list of row-invariant arguments)
#'        and `row_args` (data.frame or data.table with one row per point
#'        process). When the structured container is used it is passed as the
#'        second positional argument of both functions; the name of the second
#'        formal is up to you. A flat list keeps the released behavior
#'        (`Lambda(t, Lambda_args = ...)`, named) with a deprecation warning.
#' @param Lambda_inv_args (list) deprecated; pass one structured `Lambda_args`
#'        container used by both `Lambda` and `Lambda_inv`.
#' @param t_min (scalar | vector | column matrix) is the lower bound
#'        of a subinterval of (rate_matrix_t_min, rate_matrix_t_max]. If set,
#'        times are sampled from the subinterval.
#'        If omitted, it is equivalent to `rate_matrix_t_min`.
#' @param t_max (scalar | vector | column matrix) is the upper bound
#'        of a subinterval of (rate_matrix_t_min, rate_matrix_t_max]. If set,
#'        times are sampled from the subinterval.
#'        If omitted, it is equivalent to `rate_matrix_t_max`.
#' @param rate_matrix_t_min (scalar | vector | column matrix) is the lower bound
#'        of the time interval for each row of (Lambda|lambda)_maj_matrix.
#'        The length of this argument is the number of point processes that should be drawn.
#' @param rate_matrix_t_max (scalar | vector | column matrix) the upper bound
#'        of the time interval for each row of (Lambda|lambda)_maj_matrix.
#'        The length of this argument is the number of point processes that should be drawn.
#' @param tol (scalar, double) tolerance for the number of events
#' @param atmost1 boolean, report at most 1 event time (alias for
#'        `report_first_K = 1`)
#' @param report_first_K `NULL` or a positive integer: report only the
#'        earliest K event times (reporting truncation).
#' @param report_last_K `NULL` or a positive integer: report only the latest
#'        K event times (ascending order; reporting truncation).
#' @param atleast1 boolean, condition on at least 1 event (alias for
#'        `generate_at_least_K = 1`)
#' @param generate_at_least_K `NULL` or a positive integer: condition the
#'        sampled process on at least K events.
#' @param generate_at_most_K `NULL` or a positive integer: condition the
#'        sampled process on at most K events.
#' @param budget_cap `NULL` or a positive integer: cap the computational event
#'        budget of the kernel (approximation knob, not an exact contract).
#' @param atmostB deprecated alias for `budget_cap`.

#'
#' @return a vector of event times
#' @examples
#' # thinning (lambda and a majorizer matrix)
#' x <- vdraw(
#'   lambda = function(t, ...) 1 + sin(t),
#'   lambda_maj_matrix = matrix(rep(2, 50), nrow = 10),
#'   rate_matrix_t_min = 0,
#'   rate_matrix_t_max = 5
#' )
#'
#' # inversion (Lambda and its inverse), conditioned on at least one event
#' x <- vdraw(
#'   Lambda = function(t) 2 * t,
#'   Lambda_inv = function(z) z / 2,
#'   t_min = 0, t_max = rep(5, 10),
#'   atleast1 = TRUE
#' )
#' @export
vdraw <- function(
    lambda = NULL,
    lambda_args = NULL,
    Lambda_maj_matrix = NULL,
    lambda_maj_matrix = NULL,
    Lambda = NULL,
    Lambda_inv = NULL,
    Lambda_args = NULL,
    Lambda_inv_args = NULL,
    t_min = NULL,
    t_max = NULL,
    rate_matrix_t_min = NULL,
    rate_matrix_t_max = NULL,
    tol = 10^-6,
    atmost1 = FALSE,
    report_first_K = NULL,
    report_last_K = NULL,
    atleast1 = FALSE,
    generate_at_least_K = NULL,
    generate_at_most_K = NULL,
    budget_cap = NULL,
    atmostB = NULL) {
  if (!is.null(lambda) &&
    !(is.null(lambda_maj_matrix) && is.null(Lambda_maj_matrix))) {
    return(
      vdraw_intensity(
        lambda = lambda,
        lambda_args = lambda_args,
        Lambda_maj_matrix = Lambda_maj_matrix,
        lambda_maj_matrix = lambda_maj_matrix,
        rate_matrix_t_min = rate_matrix_t_min,
        rate_matrix_t_max = rate_matrix_t_max,
        t_min = t_min,
        t_max = t_max,
        tol = tol,
        atmost1 = atmost1,
        report_first_K = report_first_K,
        report_last_K = report_last_K,
        atleast1 = atleast1,
        generate_at_least_K = generate_at_least_K,
        generate_at_most_K = generate_at_most_K,
        budget_cap = budget_cap,
        atmostB = atmostB
      )
    )
  }

  return(
    vdraw_cumulative_intensity(
      Lambda = Lambda,
      Lambda_inv = Lambda_inv,
      t_min = t_min,
      t_max = t_max,
      Lambda_args = Lambda_args,
      Lambda_inv_args = Lambda_inv_args,
      tol = tol,
      atmost1 = atmost1,
      report_first_K = report_first_K,
      report_last_K = report_last_K,
      atleast1 = atleast1,
      generate_at_least_K = generate_at_least_K,
      generate_at_most_K = generate_at_most_K
    )
  )
}
