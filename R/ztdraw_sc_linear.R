#' Simulate `size` samples from a zero-truncated non homogeneous Poisson Point Process (zt-NHPPP) from
#'    (t_min, t_max) with linear intensity function
#'
#' @description  Sample zero-truncated NHPPP times from a linear intensity function
#' using the inversion method, optionally using an `rstream`
#' generator
#'
#' @param intercept (double) the intercept
#' @param slope (double) the slope
#' @param t_min (double) the lower bound of the time interval
#' @param t_max (double) the upper bound of the time interval
#' @param atmost1 (boolean) report at most 1 event time (alias for `atmostK = 1`)
#' @param atmostK `NULL` or a positive integer: report only the earliest K
#'        event times of the conditioned process. Generalizes `atmost1`.
#' @param atleastK positive integer: condition on at least K events in the
#'        interval. `atleastK = 1` (default) is the zero-truncated process.
#'
#' @return a vector of at least `atleastK` event times
#' @export
#'
#' @examples
#' x <- ztdraw_sc_linear(intercept = 0, slope = 0.2, t_min = 0, t_max = 10)
#'
ztdraw_sc_linear <- function(intercept,
                             slope,
                             t_min,
                             t_max,
                             atmost1 = FALSE,
                             atmostK = NULL,
                             atleastK = 1) {
  if ((slope <= 0 && intercept <= 0) || (intercept + slope * t_min < 0)) {
    return(c())
  }
  if (slope == 0) {
    return(ztppp(
      rate = intercept, t_min = t_min, t_max = t_max,
      atmost1 = atmost1, atmostK = atmostK, atleastK = atleastK
    ))
  }
  if (slope < 0) {
    t_upper <- -intercept / slope
    t_max <- min(t_max, t_upper)
  }
  return(
    ztdraw_cumulative_intensity(
      Lambda = function(t) Lambda_linear_form(t, intercept = intercept, slope = slope, t0 = t_min),
      Lambda_inv = function(z) Lambda_inv_linear_form(z, intercept = intercept, slope = slope, t0 = t_min),
      t_min = t_min,
      t_max = t_max,
      atmost1 = atmost1,
      atmostK = atmostK,
      atleastK = atleastK
    )
  )
}
