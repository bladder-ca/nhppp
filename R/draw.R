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
#' @param atmost1 boolean, draw at most 1 event time
#' @param atleast1 boolean, draw at least 1 event time in interval
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
    atleast1 = FALSE) {
  if (!is.null(Lambda)) {
    if (atleast1) {
      func <- ztdraw_cumulative_intensity
    } else {
      func <- draw_cumulative_intensity
    }
    return(func(
      Lambda = Lambda,
      Lambda_inv = Lambda_inv,
      t_min = t_min,
      t_max = t_max,
      atmost1 = atmost1
    ))
  }

  if (!is.null(lambda)) {
    if (atleast1) {
      func <- ztdraw_intensity
    } else {
      func <- draw_intensity
    }
    return(func(
      lambda = lambda,
      line_majorizer_intercept = line_majorizer_intercept,
      line_majorizer_slope = line_majorizer_slope,
      line_majorizer_is_loglinear = line_majorizer_is_loglinear,
      step_majorizer_vector = step_majorizer_vector,
      t_min = t_min,
      t_max = t_max,
      atmost1 = atmost1
    ))
  }
}
