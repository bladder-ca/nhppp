#' Simulate from a zero-truncated non homogeneous Poisson Point Process (zt-NHPPP) from
#'    (t_min, t_max) with a log-linear intensity function
#'
#' @description  Sample zt-NHPPP times from an log-linear intensity function
#'
#' @param intercept (double) the intercept in the exponent
#' @param slope (double) the slope in the exponent
#' @param t_min (double) the lower bound of the time interval
#' @param t_max (double) the upper bound of the time interval
#' @param atmost1 boolean, report at most 1 event time (alias for `atmostK = 1`)
#' @param atmostK `NULL` or a positive integer: report only the earliest K
#'        event times of the conditioned process. Generalizes `atmost1`.
#' @param atleastK positive integer: condition on at least K events in the
#'        interval. `atleastK = 1` (default) is the zero-truncated process.
#'
#' @return a vector of at least `atleastK` event times
#' @export
#'
#' @examples
#' x <- ztdraw_sc_loglinear(intercept = 0, slope = 0.2, t_min = 0, t_max = 10)
#'
ztdraw_sc_loglinear <- function(intercept,
                                slope,
                                t_min,
                                t_max,
                                atmost1 = FALSE,
                                atmostK = NULL,
                                atleastK = 1) {
  if (slope == 0) {
    return(ztppp(
      rate = exp(intercept), t_min = t_min, t_max = t_max,
      atmost1 = atmost1, atmostK = atmostK, atleastK = atleastK
    ))
  }
  return(
    ztdraw_cumulative_intensity(
      Lambda = function(t) Lambda_exp_form(t, intercept = intercept, slope = slope, t0 = t_min),
      Lambda_inv = function(z) Lambda_inv_exp_form(z, intercept = intercept, slope = slope, t0 = t_min),
      t_min = t_min,
      t_max = t_max,
      atmost1 = atmost1,
      atmostK = atmostK,
      atleastK = atleastK
    )
  )
}
