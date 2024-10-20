#' Simulate from a non homogeneous Poisson Point Process (NHPPP) from
#'    (t0, t_max) (thinning method)
#'
#' @description Sample NHPPP times using the thinning method
#'
#' @param lambda (function) the instantaneous rate of the NHPPP.
#' @param majorizer_intercept (double) the intercept (`alpha`) of the [log]linear majorizer function.
#' @param majorizer_slope (double) the slope (`beta') of the [log]linear majorizer function.
#' @param t_min (double) the lower bound of the time interval.
#' @param t_max (double) the upper bound of the time interval.
#' @param majorizer_is_loglinear (boolean) if `TRUE` the majorizer is loglinear `exp(alpha + beta * t)`
#' @param atmost1 boolean, draw at most 1 event time
#'
#' @return a vector of event times (t_); if no events realize,
#'         a vector of length 0
#' @keywords internal
draw_intensity_line <- function(lambda,
                                majorizer_intercept,
                                majorizer_slope,
                                t_min,
                                t_max,
                                majorizer_is_loglinear = FALSE,
                                atmost1 = FALSE) {
  if (isTRUE(majorizer_is_loglinear)) {
    nhppp_t <- draw_sc_loglinear
    link <- exp
  } else {
    nhppp_t <- draw_sc_linear
    link <- identity
  }

  candidate_times <- nhppp_t(intercept = majorizer_intercept, slope = majorizer_slope, t_min = t_min, t_max = t_max, atmost1 = FALSE)
  num_candidates <- length(candidate_times)
  if (num_candidates == 0) {
    return(candidate_times)
  }
  u <- stats::runif(n = num_candidates, min = 0, max = 1)
  acceptance_prob <- lambda(candidate_times) / link(majorizer_intercept + majorizer_slope * candidate_times)
  if (!all(acceptance_prob <= 1 + 10^-6)) {
    stop("lambda > lambda_maj\n")
  }
  if (atmost1) {
    t <- candidate_times[u < acceptance_prob][1]
    if (is.na(t)) {
      t <- numeric(0)
    }
  } else {
    t <- candidate_times[u < acceptance_prob]
  }
  return(t)
}
