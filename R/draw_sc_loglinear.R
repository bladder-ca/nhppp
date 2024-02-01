#' Special case: Simulate from a non homogeneous Poisson Point Process (NHPPP) from
#'    (t_min, t_max) with log-linear intensity function (inversion method)
#'
#' @description  Sample NHPPP times from an log linear intensity function
#' using the inversion method, optionally using an `rstream`
#' generator
#'
#' @param alpha (double) the intercept in the exponent
#' @param beta (double) the slope in the exponent
#' @param range_t (vector, double) min and max of the time interval
#' @param rng_stream (`rstream`) an `rstream` object.
#' @param atmost1 boolean, draw at most 1 event time
#'
#' @return a vector of event times (t_); if no events realize,
#'         a vector of length 0
#' @export
#'
#' @examples
#' x <- draw_sc_loglinear(alpha = 0, beta = 0.2)
#'
draw_sc_loglinear <- function(alpha = 1,
                              beta = 0,
                              range_t = c(0, 10),
                              rng_stream = NULL,
                              atmost1 = FALSE) {
  if (beta == 0) {
    return(ppp_orderstat(range_t = range_t, rate = exp(alpha), rng_stream = rng_stream, atmost1 = atmost1))
  }
  return(
    draw_cumulative_intensity_inversion(
      Lambda = function(t) Lambda_exp_form(t, alpha = alpha, beta = beta, t0 = range_t[1]),
      Lambda_inv = function(z) Lambda_inv_exp_form(z, alpha = alpha, beta = beta, t0 = range_t[1]),
      range_t = range_t,
      rng_stream = rng_stream,
      atmost1 = atmost1
    )
  )
}
