#' Simulate `size` samples from a zero-truncated non homogeneous Poisson Point Process (zt-NHPPP) from
#'    (t_min, t_max) with linear intensity function
#'
#' @description  Sample zero-truncated NHPPP times from a linear intensity function
#' using the inversion method, optionally using an `rstream`
#' generator
#'
#' @param alpha (double) the intercept
#' @param beta (double) the slope
#' @param range_t (vector, double) min and max of the time interval
#' @param rng_stream (`rstream`) an `rstream` object
#' @param atmost1 (boolean) draw 1 event time
#'
#' @return a vector of at least 1 event times
#' @export
#'
#' @examples
#' x <- ztdraw_sc_linear(alpha = 0, beta = 0.2)
#'
ztdraw_sc_linear <- function(alpha = 1,
                             beta = 0,
                             range_t = c(0, 10),
                             rng_stream = NULL,
                             atmost1 = FALSE) {
  if ((beta <= 0 && alpha <= 0) || (alpha + beta * range_t[1] < 0)) {
    return(c())
  }
  if (beta == 0) {
    return(ztppp(range_t = range_t, rate = alpha, rng_stream = rng_stream, atmost1 = atmost1))
  }
  if (beta < 0) {
    t_max <- -alpha / beta
    range_t[2] <- min(range_t[2], t_max)
  }
  return(
    ztdraw_cumulative_intensity(
      Lambda = function(t) Lambda_linear_form(t, alpha = alpha, beta = beta, t0 = range_t[1]),
      Lambda_inv = function(z) Lambda_inv_linear_form(z, alpha = alpha, beta = beta, t0 = range_t[1]),
      range_t = range_t,
      range_L = Lambda_linear_form(range_t, alpha = alpha, beta = beta, t0 = range_t[1]),
      rng_stream = rng_stream,
      atmost1 = atmost1
    )
  )
}
