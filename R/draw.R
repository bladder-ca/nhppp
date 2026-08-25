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
#' @param atmost1 boolean, report at most 1 event time (alias for `atmostK = 1`)
#' @param atmostK `NULL` or a positive integer: report only the earliest K
#'        event times. Generalizes `atmost1`.
#' @param atleast1 boolean, condition on at least 1 event (alias for `atleastK = 1`)
#' @param atleastK `NULL` or a positive integer: condition on at least K events
#'        in the interval. Generalizes `atleast1`. With `lambda` (thinning),
#'        only `atleastK = 1` is implemented.
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
    atmostK = NULL,
    atleast1 = FALSE,
    atleastK = NULL) {
  atmostK <- .resolve_atmostK(atmost1, atmostK)
  atleastK <- .resolve_atleastK(atleast1, atleastK)

  if (!is.null(Lambda)) {
    if (atleastK >= 1L) {
      return(ztdraw_cumulative_intensity(
        Lambda = Lambda,
        Lambda_inv = Lambda_inv,
        t_min = t_min,
        t_max = t_max,
        atmostK = if (atmostK > 0L) atmostK else NULL,
        atleastK = atleastK
      ))
    }
    z <- draw_cumulative_intensity(
      Lambda = Lambda,
      Lambda_inv = Lambda_inv,
      t_min = t_min,
      t_max = t_max,
      atmost1 = (atmostK == 1L)
    )
    if (atmostK > 1L && length(z) > atmostK) z <- z[seq_len(atmostK)]
    return(z)
  }

  if (!is.null(lambda)) {
    if (atleastK >= 2L) {
      stop("`atleastK >= 2` has not been implemented for the thinning-based (intensity) samplers.")
    }
    if (atleastK == 1L) {
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
      atmost1 = (atmostK == 1L)
    )
    if (atmostK > 1L && length(z) > atmostK) z <- z[seq_len(atmostK)]
    return(z)
  }
}
