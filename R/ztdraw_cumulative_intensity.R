#' Simulate from a zero-truncated non homogeneous Poisson Point Process (zt-NHPPP) from
#'    (t_min, t_max) (order statistics method)
#'
#' @description  Sample zero-truncated NHPPP times using the order statistics method,
#' optionally using an `rstream` generator
#' @param Lambda (function, double vector) a continuous increasing R to R map
#'               which is the integrated rate of the NHPPP
#' @param Lambda_inv (function, double vector) the inverse of `Lambda()`
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
ztdraw_cumulative_intensity <- function(Lambda,
                                        Lambda_inv,
                                        t_min,
                                        t_max,
                                        atmost1 = FALSE,
                                        report_first_K = NULL,
                                        report_last_K = NULL,
                                        generate_at_least_K = 1,
                                        generate_at_most_K = NULL) {
  # validation and reporting happen inside ztppp(); the monotone Lambda_inv
  # map preserves order, so reporting commutes with the transformation
  tmp_u <- ztppp(
    rate = 1, t_min = Lambda(t_min), t_max = Lambda(t_max),
    atmost1 = atmost1,
    report_first_K = report_first_K, report_last_K = report_last_K,
    generate_at_least_K = generate_at_least_K,
    generate_at_most_K = generate_at_most_K
  )
  return(Lambda_inv(tmp_u))
}
