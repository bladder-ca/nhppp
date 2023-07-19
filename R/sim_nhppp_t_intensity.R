#' Simulate from a non homogeneous Poisson Point Process (NHPPP) from
#'    (t0, t_max) (thinning method)
#'
#' @description Sample NHPPP times using the thinning method, optionally using
#' an `rstream` generator or a `Kystis` `RNGClass` object.
#' @param lambda (function) the instantaneous rate of the NHPPP.
#' A continuous function of time.
#' @param lambda_maj (double, vector) the intercept and optional slope of the majorizing
#' linear (if `exp_maj = FALSE`) or exponential linear (if `exp_maj = TRUE`) function in `range_t`.
#' @param exp_maj (boolean) if `TRUE` the majorizer is `exp(alpha + beta * t)`
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
                              exp_maj = FALSE,
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

  if (isTRUE(exp_maj)) {
    nhppp_t <- nhppp_t_intensity_exponential
    link <- exp
  } else {
    nhppp_t <- nhppp_t_intensity_linear
    link <- identity
  }

  candidate_times <- nhppp_t(alpha = alpha, beta = beta, range_t = range_t, rng_stream = rng_stream, only1 = FALSE)
  num_candidates <- length(candidate_times)
  if (num_candidates == 0) {
    return(candidate_times)
  }
  u <- rng_stream_runif(size = num_candidates, minimum = 0, maximum = 1, rng_stream = rng_stream)
  acceptance_prob <- lambda(candidate_times) / link(alpha + beta * candidate_times)
  stopifnot(all(acceptance_prob <= 1 + 10^-6))
  if (only1) {
    t <- candidate_times[u < acceptance_prob][1]
    if (is.na(t)) {t <- numeric(0)}
  } else {
   t <- candidate_times[u < acceptance_prob]
  }
  return(t)
}




#' Simulate from a non homogeneous Poisson Point Process (NHPPP) from
#'    (t0, t_max) (thinning method) with piecewise constant_majorizer
#'
#' @description Sample NHPPP times using the thinning method, optionally using
#' an `rstream` generator or a `Kystis` `RNGClass` object.
#' @param lambda (function) the instantaneous rate of the NHPPP.
#' A continuous function of time.
#' @param lambda_maj_vector (scalar, double) `K` constant majorizing rates, one per interval
#' @param times_vector (vector, double) `K+1` time points defining `K` intervals
#'        of constant rates:
#'             `[t_1 = range_t[1],       t_2)`: the first interval
#'             `[t_k,                t_{k+1})`: the `k`-th interval
#'             `[t_{K}, t_{K+1} = range_t[2])`: the `K`-th (last) interval
#' @param rng_stream (`rstream`) an `rstream` or `RNGClass` object, or `NULL`
#' @param only1 boolean, draw at most 1 event time
#'
#' @return a vector of event times (t_); if no events realize,
#'         a vector of length 0
#' @export
#'
#' @examples
#' x <- nhppp_t_intensity_piecewise(lambda = function(t) exp(.02*t))
nhppp_t_intensity_piecewise <- function(lambda,
                              lambda_maj_vector = 1,
                              times_vector = c(0, 10),
                              rng_stream = NULL,
                              only1 = FALSE) {

  len_lambda <- length(lambda_maj_vector)
  candidate_times <- ppp_t_piecewise(rates_vector = lambda_maj_vector, times_vector = times_vector, rng_stream = rng_stream, only1 = FALSE)
  num_candidates <- length(candidate_times)
  if (num_candidates == 0) {
    return(candidate_times)
  }
  u <- rng_stream_runif(size = num_candidates, minimum = 0, maximum = 1, rng_stream = rng_stream)
  acceptance_prob <- lambda(candidate_times) /
    approx(x = times_vector[1:len_lambda],
           y = lambda_maj_vector,
           xout = candidate_times, method = "constant", rule = 2, f = 0)$y
  stopifnot(all(acceptance_prob <= 1 + 10^-6))
  if (only1) {
    t <- candidate_times[u < acceptance_prob][1]
    if (is.na(t)) {t <- numeric(0)}
  } else {
    t <- candidate_times[u < acceptance_prob]
  }
  return(t)
}
