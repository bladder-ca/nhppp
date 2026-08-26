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
#' @param atmost1 (boolean) report at most 1 event time of the conditioned
#'        process (alias for `report_first_K = 1`)
#' @param report_first_K `NULL` or a positive integer: report only the
#'        earliest K event times of the conditioned realization.
#' @param report_last_K `NULL` or a positive integer: report only the latest
#'        K event times of the conditioned realization (ascending order).
#' @param generate_at_least_K non-negative integer: condition on at least K
#'        events in the interval. The default 1 is the zero-truncated process.
#' @param generate_at_most_K `NULL` or a positive integer: condition on at
#'        most K events.
#'
#' @return a vector of event times of the conditioned process
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
                             report_first_K = NULL,
                             report_last_K = NULL,
                             generate_at_least_K = 1,
                             generate_at_most_K = NULL) {
  if ((slope <= 0 && intercept <= 0) || (intercept + slope * t_min < 0)) {
    return(c())
  }
  if (slope == 0) {
    return(ztppp(
      rate = intercept, t_min = t_min, t_max = t_max,
      atmost1 = atmost1,
      report_first_K = report_first_K, report_last_K = report_last_K,
      generate_at_least_K = generate_at_least_K,
      generate_at_most_K = generate_at_most_K
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
      report_first_K = report_first_K,
      report_last_K = report_last_K,
      generate_at_least_K = generate_at_least_K,
      generate_at_most_K = generate_at_most_K
    )
  )
}
