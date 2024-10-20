#' Generic function for simulating from zero-truncated NHPPPs given the intensity function.
#'
#' @description Sample from zero-truncated NHPPP given the intensity function
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
#' @param atmost1 boolean, draw at most 1 event time
#'
#' @return a vector of at least 1 event times
#' @keywords internal
ztdraw_intensity <- function(
    lambda,
    line_majorizer_intercept = NULL,
    line_majorizer_slope = NULL,
    line_majorizer_is_loglinear = FALSE,
    step_majorizer_vector = NULL,
    t_min = NULL,
    t_max = NULL,
    atmost1 = FALSE) {
  if (!is.null(step_majorizer_vector)) {
    return(
      ztdraw_intensity_step(
        lambda = lambda,
        majorizer_vector = step_majorizer_vector,
        time_breaks = seq.int(from = t_min, to = t_max, length.out = length(step_majorizer_vector) + 1),
        atmost1 = atmost1
      )
    )
  }
  return(
    ztdraw_intensity_line(
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
