#' Generic function for simulating from NHPPPs given the intensity function
#' or the cumulative intensity function.
#'
#' @description
#' This is a wrapper to the package's specific functions, and thus somewhat slower.
#' For time-intensive simulations prefer one of the specific functions.
#'
#' @param Lambda (function, double vector) the integrated (cumulative) rate of the NHPPP
#' @param Lambda_inv (function, double vector) the inverse of `Lambda()'
#' @param lambda (function) the instantaneous rate
#' @param line_majorizer_intercept The intercept `alpha` of the [log]linear majorizer function: `alpha + beta * t` or `exp(alpha + beta * t)`
#' @param line_majorizer_slope The slope `beta` of the [log]linear majorizer function: `alpha + beta * t` or `exp(alpha + beta * t)`
#' @param line_majorizer_is_loglinear (boolean) if `TRUE` the majorizer is loglinear `exp(alpha + beta * t)`; if `FALSE` it is a linear function
#' @param step_majorizer_vector (vector, double) `K` constant majorizing rates, one per interval; all intervals are of equal length (regular)
#' @param t_min (double) the lower bound of the interval
#' @param t_max (double) the upper bound of the interval
#' @param atmost1 boolean, report at most 1 event time (alias for
#'        `report_first_K = 1`)
#' @param report_first_K `NULL` or a positive integer: report only the
#'        earliest K event times (reporting truncation).
#' @param report_last_K `NULL` or a positive integer: report only the latest
#'        K event times (ascending order; reporting truncation).
#' @param atleast1 boolean, condition on at least 1 event (alias for
#'        `generate_at_least_K = 1`)
#' @param generate_at_least_K `NULL` or a positive integer: condition the
#'        sampled process on at least K events. With `lambda` (thinning),
#'        only `generate_at_least_K = 1` is implemented.
#' @param generate_at_most_K `NULL` or a positive integer: condition the
#'        sampled process on at most K events. Not implemented with `lambda`
#'        (thinning).
#'
#' @return a vector of event times
#' @export
draw <- function(
    Lambda = NULL,
    Lambda_inv = NULL,
    lambda = NULL,
    line_majorizer_intercept = NULL,
    line_majorizer_slope = NULL,
    line_majorizer_is_loglinear = FALSE,
    step_majorizer_vector = NULL,
    t_min = NULL,
    t_max = NULL,
    atmost1 = FALSE,
    report_first_K = NULL,
    report_last_K = NULL,
    atleast1 = FALSE,
    generate_at_least_K = NULL,
    generate_at_most_K = NULL) {
  rep_ <- .resolve_reporting(atmost1, report_first_K, report_last_K)
  gen_ <- .resolve_generation(atleast1, generate_at_least_K, generate_at_most_K)
  conditioned <- (gen_$at_least > 0L || gen_$at_most > 0L)

  if (!is.null(Lambda)) {
    if (conditioned) {
      return(ztdraw_cumulative_intensity(
        Lambda = Lambda,
        Lambda_inv = Lambda_inv,
        t_min = t_min,
        t_max = t_max,
        report_first_K = if (rep_$first > 0L) rep_$first else NULL,
        report_last_K = if (rep_$last > 0L) rep_$last else NULL,
        generate_at_least_K = if (gen_$at_least > 0L) gen_$at_least else NULL,
        generate_at_most_K = if (gen_$at_most > 0L) gen_$at_most else NULL
      ))
    }
    z <- draw_cumulative_intensity(
      Lambda = Lambda,
      Lambda_inv = Lambda_inv,
      t_min = t_min,
      t_max = t_max,
      atmost1 = (rep_$first == 1L)
    )
    return(.report_slice_vector(z, rep_))
  }

  if (!is.null(lambda)) {
    if (gen_$at_least >= 2L || gen_$at_most > 0L) {
      stop("only `generate_at_least_K = 1` has been implemented for the scalar thinning-based (intensity) samplers.")
    }
    if (gen_$at_least == 1L) {
      func <- ztdraw_intensity
    } else {
      func <- draw_intensity
    }
    z <- func(
      lambda = lambda,
      line_majorizer_intercept = line_majorizer_intercept,
      line_majorizer_slope = line_majorizer_slope,
      line_majorizer_is_loglinear = line_majorizer_is_loglinear,
      step_majorizer_vector = step_majorizer_vector,
      t_min = t_min,
      t_max = t_max,
      atmost1 = (rep_$first == 1L)
    )
    return(.report_slice_vector(z, rep_))
  }
}
