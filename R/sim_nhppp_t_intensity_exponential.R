#' Simulate from a non homogeneous Poisson Point Process (NHPPP) from
#'    (t_min, t_max) with exponential intensity function (inversion method)
#'
#' @description  Sample NHPPP times from an exponential intensity function
#' using the inversion method, optionally using an `rstream`
#' generator or a `Kystis` `RNGClass` object
#'
#' @param alpha (double) the intercept in the exponent
#' @param beta (double) the slope in the exponent
#' @param range_t (vector, double) min and max of the time interval
#' @param tol the probability that we will have more than
#'        the drawn events in (t_min, t_max]
#' @param rng_stream (`rstream`) an `rstream` object.
#' @param only1 boolean, draw at most 1 event time
#'
#' @return a vector of event times (t_); if no events realize,
#'         a vector of length 0
#' @export
#'
#' @examples
#' x <- nhppp_t_intensity_exponential(alpha = 0, beta = 0.2)
#'
nhppp_t_intensity_exponential <- function(alpha = 1,
                                          beta = 0,
                                          range_t = c(0, 10),
                                          tol = 10^-6,
                                          rng_stream = NULL,
                                          only1 = FALSE) {
  if (beta == 0) {
    return(ppp_t_orderstat(range_t = range_t, rate = exp(alpha), tol = tol, rng_stream = rng_stream, only1 = only1))
  }
  return(
    nhppp_t_cumulative_intensity_inversion(
      Lambda = function(t) Lambda_exp_form(t, alpha = alpha, beta = beta, t0 = range_t[1]),
      Lambda_inv = function(z) Lambda_inv_exp_form(z, alpha = alpha, beta = beta, t0 = range_t[1]),
      range_t = range_t,
      rng_stream = rng_stream,
      only1 = only1
    )
  )
}


#' Simulate from a non homogeneous Poisson Point Process (NHPPP) from
#'    (t_min, t_max) with linear intensity function (inversion method)
#'
#' @description  Sample NHPPP times from a linear intensity function
#' using the inversion method, optionally using an `rstream`
#' generator or a `Kystis` `RNGClass` object
#'
#' @param alpha (double) the intercept
#' @param beta (double) the slope
#' @param range_t (vector, double) min and max of the time interval
#' @param tol the probability that we will have more than
#'        the drawn events in (t_min, t_max]
#' @param rng_stream (`rstream`) an `rstream` object.
#' @param only1 boolean, draw at most 1 event time
#'
#' @return a vector of event times (t_); if no events realize,
#'         a vector of length 0
#' ###noexport @export
#'
#' @examples
#' x <- nhppp_t_intensity_linear_old(alpha = 0, beta = 0.2)
#'
nhppp_t_intensity_linear_old <- function(alpha = 1,
                                         beta = 0,
                                         range_t = c(0, 10),
                                         tol = 10^-6,
                                         rng_stream = NULL,
                                         only1 = FALSE) {
  if (beta == 0) {
    stopifnot(alpha > 0)
    return(ppp_t(range_t = range_t, rate = alpha, tol = tol, rng_stream = rng_stream, only1 = only1))
  }
  stopifnot(alpha + beta * range_t[1] >= 0)
  if (beta < 0) {
    t_max <- -alpha / beta
    range_t[2] <- min(range_t[2], t_max)
  }
  return(
    nhppp_t_cumulative_intensity_inversion(
      Lambda = function(t) Lambda_linear_form(t, alpha = alpha, beta = beta, t0 = range_t[1]),
      Lambda_inv = function(z) Lambda_inv_linear_form(z, alpha = alpha, beta = beta, t0 = range_t[1]),
      range_t = range_t,
      rng_stream = rng_stream,
      only1 = only1
    )
  )
}
