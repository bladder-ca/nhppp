#' Simulate `size` samples from a non homogeneous Poisson Point Process (NHPPP) from
#'    (t_min, t_max) with linear intensity function (inversion method)
#'
#' @description  Sample NHPPP times from a linear intensity function
#' using the inversion method, optionally using an `rstream`
#' generator or a `Kystis` `RNGClass` object
#'
#' @param size (double) the number of samples
#' @param alpha (double) the intercept
#' @param beta (double) the slope
#' @param range_t (vector, double) min and max of the time interval
#' @param tol the probability that we will have more than
#'        the drawn events in (t_min, t_max]
#' @param rng_stream (`rstream`) an `rstream` object.
#'
#' @return a vector of event times (t_); if no events realize,
#'         a vector of length 0
#' @export
#'
#' @examples
#' x <- nhppp_n_intensity_linear(alpha = 0, beta = 0.2)
#'
nhppp_n_intensity_linear <- function(size,
                                     alpha = 1,
                                     beta = 0,
                                     range_t = c(0, 10),
                                     tol = 10^-6,
                                     rng_stream = NULL) {
  if ((size == 0) || (beta <= 0 && alpha <= 0) || (alpha + beta * range_t[1] < 0)) {
    return(c())
  }
  if (beta == 0) {
    return(ppp_n(size = size, range_t = range_t, rng_stream = rng_stream))
  }
  if (beta < 0) {
    t_max <- -alpha / beta
    range_t[2] <- min(range_t[2], t_max)
  }
  return(
    nhppp_n_cumulative_intensity(
      size = size,
      Lambda = function(t) Lambda_linear_form(t, alpha = alpha, beta = beta, t0 = range_t[1]),
      Lambda_inv = function(z) Lambda_inv_linear_form(z, alpha = alpha, beta = beta, t0 = range_t[1]),
      range_t = range_t,
      rng_stream = rng_stream
    )
  )
}
