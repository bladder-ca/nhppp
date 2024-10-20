#' Special case: Simulate from a non homogeneous Poisson Point Process (NHPPP) from
#'    (t_min, t_max) with log-linear intensity function (inversion method)
#'
#' @description  Sample NHPPP times from an log linear intensity function
#' using the inversion method, optionally using an `rstream`
#' generator
#'
#' @param intercept (double) the intercept in the exponent
#' @param slope (double) the slope in the exponent
#' @param t_min (double) lower bound of the time interval
#' @param t_max (double) upper bound of the time interval
#' @param atmost1 boolean, draw at most 1 event time
#'
#' @return a vector of event times (t_); if no events realize,
#'         a vector of length 0
#' @export
#'
#' @examples
#' x <- draw_sc_loglinear(intercept = 0, slope = 0.2, t_min = 0, t_max = 10)
#'
draw_sc_loglinear <- function(intercept,
                              slope,
                              t_min,
                              t_max,
                              atmost1 = FALSE) {
  if (slope == 0) {
    return(ppp2(rate = exp(intercept), t_min = t_min, t_max = t_max, atmost1 = atmost1))
  }
  return(
    draw_cumulative_intensity_inversion(
      Lambda = function(t) Lambda_exp_form(t, intercept = intercept, slope = slope, t0 = t_min),
      Lambda_inv = function(z) Lambda_inv_exp_form(z, intercept = intercept, slope = slope, t0 = t_min),
      t_min = t_min,
      t_max = t_max,
      atmost1 = atmost1
    )
  )
}
