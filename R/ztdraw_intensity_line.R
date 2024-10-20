#' Simulate `size` samples from a zero-truncated non homogeneous Poisson Point Process (zt-NHPPP) from
#'    (t0, t_max) (thinning method)
#'
#' @description Sample zero-truncated NHPPP intensity times using the thinning method
#'
#' @param lambda (function) the instantaneous rate of the NHPPP.
#' @param majorizer_intercept (double) the intercept (`alpha`) of the [log]linear majorizer function.
#' @param majorizer_slope (double) the slope (`beta') of the [log]linear majorizer function.
#' @param t_min (double) the lower bound of the time interval.
#' @param t_max (double) the upper bound of the time interval.
#' @param majorizer_is_loglinear (boolean) if `TRUE` the majorizer is loglinear `exp(alpha + beta * t)`
#' @param atmost1 boolean, draw at most 1 event time
#'
#' @return a vector of at least 1 event times
#' @keywords internal
ztdraw_intensity_line <- function(lambda,
                                  majorizer_intercept,
                                  majorizer_slope,
                                  t_min,
                                  t_max,
                                  majorizer_is_loglinear = FALSE,
                                  atmost1 = FALSE) {
  if (isTRUE(majorizer_is_loglinear)) {
    ztnhppp_t <- ztdraw_sc_loglinear
    link <- exp
  } else {
    ztnhppp_t <- ztdraw_sc_linear
    link <- identity
  }

  while (TRUE) {
    candidate_times <- ztnhppp_t(intercept = majorizer_intercept, slope = majorizer_slope, t_min = t_min, t_max = t_max, atmost1 = FALSE)
    u <- stats::runif(n = length(candidate_times), min = 0, max = 1)
    acceptance_prob <- lambda(candidate_times) / link(majorizer_intercept + majorizer_slope * candidate_times)
    if (!all(acceptance_prob <= 1 + 10^-6)) stop("lambda > lambda_maj\n")
    candidate_times <- candidate_times[u < acceptance_prob]
    if (length(candidate_times) > 0) {
      if (atmost1) {
        return(candidate_times[1])
      } else {
        return(candidate_times)
      }
    }
  }
}
