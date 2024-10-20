#' Simulate from a zero-truncated non homogeneous Poisson Point Process (zt-NHPPP) from
#'    (t_min, t_max) with a log-linear intensity function
#'
#' @description  Sample zt-NHPPP times from an log-linear intensity function
#'
#' @param intercept (double) the intercept in the exponent
#' @param slope (double) the slope in the exponent
#' @param t_min (double) the lower bound of the time interval
#' @param t_max (double) the upper bound of the time interval
#' @param atmost1 boolean, 1 event time
#'
#' @return a vector of at least 1 event times
#' @export
#'
#' @examples
#' x <- ztdraw_sc_loglinear(intercept = 0, slope = 0.2, t_min = 0, t_max = 10)
#'
ztdraw_sc_loglinear <- function(intercept,
                                slope,
                                t_min,
                                t_max,
                                atmost1 = FALSE) {
  if (slope == 0) {
    return(ztppp(rate = exp(intercept), t_min = t_min, t_max = t_max, atmost1 = atmost1))
  }
  return(
    ztdraw_cumulative_intensity(
      Lambda = function(t) Lambda_exp_form(t, intercept = intercept, slope = slope, t0 = t_min),
      Lambda_inv = function(z) Lambda_inv_exp_form(z, intercept = intercept, slope = slope, t0 = t_min),
      t_min = t_min,
      t_max = t_max,
      atmost1 = atmost1
    )
  )
}
