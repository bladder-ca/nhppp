#' Simulate from a non homogeneous Poisson Point Process (NHPPP) from
#'    (t0, t_max) (thinning method)
#'
#' @description Sample NHPPP times using the thinning method, optionally using
#' an `rstream` generator
#' @param lambda (function) the instantaneous rate of the NHPPP.
#' A continuous function of time.
#' @param majorizer_intercept (double) the intercept of the [log]linear majorizer function.
#' @param majorizer_slope (double) the slope of the [log]linear majorizer function.
#' @param t_min (double) the lower bound of the time interval.
#' @param t_max (double) the upper bound of the time interval.
#' @param loglinear_majorizer (boolean) if `TRUE` the majorizer is loglinear `exp(alpha + beta * t)`
#' #@param range_t (vector, double) min and max of the time interval.
#' #@param rng_stream (`rstream`) an `rstream` object or `NULL`
#' @param atmost1 boolean, draw at most 1 event time
#'
#' @return a vector of event times (t_); if no events realize,
#'         a vector of length 0
#' @export
#'
#' @examples
#' x <- draw_intensity(lambda = function(t) 2, majorizer_intercept = 2.2, majorizer_slope = 0.1, t_min = 1, t_max = 5)
draw_intensity <- function(lambda,
                           majorizer_intercept,
                           majorizer_slope,
                           t_min, 
                           t_max,
                           loglinear_majorizer = FALSE,
                           # range_t = c(0, 10),
                           # rng_stream = NULL,
                           atmost1 = FALSE) {
  
  if (isTRUE(loglinear_majorizer)) {
    nhppp_t <- draw_sc_loglinear
    link <- exp
  } else {
    nhppp_t <- draw_sc_linear
    link <- identity
  }

  candidate_times <- nhppp_t(alpha = majorizer_intercept, beta = majorizer_slope, t_min = t_min, t_max = t_max, atmost1 = FALSE)
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
