#' Simulate from a non homogeneous Poisson Point Process (NHPPP) from
#'    (t0, t_max) (thinning method)
#'
#' @description Sample NHPPP times using the thinning method, optionally using
#' an `rstream` generator or a `Kystis` `RNGClass` object.
#' @param lambda (function) the instantaneous rate of the NHPPP.
#' A continuous function of time.
#' @param lambda_maj (double, vector) the intercept and optional slope of the majorizing
#' linear function in `range_t`.
#' @param range_t (vector, double) min and max of the time interval.
#' @param rng_stream (`rstream`) an `rstream` or `RNGClass` object, or `NULL`
#' @param only1 boolean, draw at most 1 event time
#'
#' @return a vector of event times (t_); if no events realize,
#'         a vector of length 0
#' @export
#'
#' @examples
#' x <- nhppp_t_intensity(lambda = function(t) 1 + sin(t))
nhppp_t_intensity <- function(lambda,
                              lambda_maj = NULL,
                              range_t = c(0, 10),
                              rng_stream = NULL,
                              only1 = FALSE) {
  if (is.null(lambda_maj)) {
    alpha <- stats::optimize(
      f = function(x) lambda(x),
      interval = range_t,
      maximum = TRUE
    )$objective * 1.2
    beta <- 0
  } else if (length(lambda_maj) == 1) {
    alpha <- lambda_maj[1]
    beta <- 0
  } else if (length(lambda_maj) == 2) {
    alpha <- lambda_maj[1]
    beta <- lambda_maj[2]
  }

  candidate_times <- nhppp_t_intensity_linear(alpha = alpha, beta = beta, range_t = range_t, rng_stream = rng_stream, only1 = FALSE)
  num_candidates <- length(candidate_times)
  if (num_candidates == 0) {
    return(candidate_times)
  }
  u <- rng_stream_runif(size = num_candidates, minimum = 0, maximum = 1, rng_stream = rng_stream)
  acceptance_prob <- lambda(candidate_times) / (alpha + beta * candidate_times)
  stopifnot(all(acceptance_prob <= 1))
  if (only1) {
    return(candidate_times[u < acceptance_prob][1])
  } else {
    return(candidate_times[u < acceptance_prob])
  }
}
