#' Simulate from a zero-truncated non homogeneous Poisson Point Process (zt-NHPPP) from
#'    (t_min, t_max) with a log-linear intensity function (inversion method)
#'
#' @description  Sample zt-NHPPP times from an log-linear intensity function
#' using the inversion method, optionally using an `rstream`
#' generator
#'
#' @param alpha (double) the intercept in the exponent
#' @param beta (double) the slope in the exponent
#' @param range_t (vector, double) min and max of the time interval
#' @param rng_stream (`rstream`) an `rstream` object.
#' @param atmost1 boolean, 1 event time
#'
#' @return a vector of at least 1 event times
#' @export
#'
#' @examples
#' x <- ztdraw_sc_loglinear(alpha = 0, beta = 0.2)
#'
ztdraw_sc_loglinear <- function(alpha = 1,
                                beta = 0,
                                range_t = c(0, 10),
                                rng_stream = NULL,
                                atmost1 = FALSE) {
  if (beta == 0) {
    return(ztppp(range_t = range_t, rate = exp(alpha), rng_stream = rng_stream, atmost1 = atmost1))
  }
  return(
    draw_cumulative_intensity_inversion(
      Lambda = function(t) Lambda_exp_form(t, alpha = alpha, beta = beta, t0 = range_t[1]),
      Lambda_inv = function(z) Lambda_inv_exp_form(z, alpha = alpha, beta = beta, t0 = range_t[1]),
      range_t = range_t,
      range_L = Lambda_exp_form(range_t, alpha = alpha, beta = beta, t0 = range_t[1]),
      rng_stream = rng_stream,
      atmost1 = atmost1
    )
  )
}
