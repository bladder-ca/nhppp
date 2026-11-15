#' Generic function for simulating from NHPPPs given the intensity function.
#'
#' @description Sample from NHPPPs given the intensity function
#' This is a wrapper to the package's specific functions, and thus somewhat slower.
#' For time-intensive simulations prefer one of the specific functions.
#'
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
#'        earliest K event times (reporting truncation — the count law is
#'        unchanged).
#' @param report_last_K `NULL` or a positive integer: report only the latest
#'        K event times (ascending order; reporting truncation).
#' @param atleast1 boolean, condition on at least 1 event (alias for
#'        `generate_at_least_K = 1`)
#' @param generate_at_least_K `NULL` or `1`: condition the sampled process on
#'        at least 1 event. Larger values are not implemented for the scalar
#'        thinning sampler and raise an error.
#' @param generate_at_most_K must be `NULL`; conditioning on at most K events
#'        is not implemented for the scalar thinning sampler.
#'
#' @return a vector of event times
#' @export
draw_intensity <- function(
    lambda,
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
  if (gen_$at_least >= 2L || gen_$at_most > 0L) {
    stop("only `generate_at_least_K = 1` has been implemented for the scalar thinning-based (intensity) samplers.")
  }
  func <- if (gen_$at_least == 1L) ztdraw_intensity else draw_intensity_plain
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

#' Unconditioned thinning sampler behind `draw_intensity()`
#' @noRd
draw_intensity_plain <- function(lambda,
                                 line_majorizer_intercept = NULL,
                                 line_majorizer_slope = NULL,
                                 line_majorizer_is_loglinear = FALSE,
                                 step_majorizer_vector = NULL,
                                 t_min = NULL,
                                 t_max = NULL,
                                 atmost1 = FALSE) {
  if (!is.null(step_majorizer_vector)) {
    return(
      draw_intensity_step(
        lambda = lambda,
        majorizer_vector = step_majorizer_vector,
        time_breaks = seq.int(from = t_min, to = t_max, length.out = length(step_majorizer_vector) + 1),
        atmost1 = atmost1
      )
    )
  }
  return(
    draw_intensity_line(
      lambda = lambda,
      majorizer_intercept = line_majorizer_intercept,
      majorizer_slope = line_majorizer_slope,
      majorizer_is_loglinear = line_majorizer_is_loglinear,
      t_min = t_min,
      t_max = t_max,
      atmost1 = atmost1
    )
  )
}
