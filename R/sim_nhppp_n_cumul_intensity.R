#' Simulate `size` samples from a non homogeneous Poisson Point Process (NHPPP) in the interval
#'    (t_min, t_max)
#'
#' @description  Sample NHPPP times using the order statistics method,
#' optionally using an `rstream` generator or a `Kystis` `RNGClass` object.
#' @param size int, the number of samples needed
#' @param Lambda (function, double vector) a continuous increasing R to R map
#'               which is the integrated rate of the NHPPP
#' @param Lambda_inv (function, double vector) the inverse of `Lambda()`
#' @param range_t (vector, double) min and max of the time interval
#' @param range_L (vector, double) min and max of the transformed time interval
#' @param rng_stream (`rstream`) an `rstream`, `RNGClass` object or `NULL`.
#'
#' @return a vector of `size` event times
#' @export
#'
#' @examples
#' x <- nhppp_n_cumulative_intensity(23, Lambda = function(t) t + cos(t) - 1)
nhppp_n_cumulative_intensity <- function(size, Lambda,
                                         Lambda_inv = NULL,
                                         range_t = c(0, 10),
                                         range_L = c(Lambda(range_t[1]), Lambda(range_t[2])),
                                         rng_stream = NULL) {
  #tmp_u <- ppp_n(size = size, range_t = range_L, rng_stream = rng_stream)
  tmp_u <- ppp_n(size = size, range_t = c(0,1), rng_stream = rng_stream)
  
  if (is.function(Lambda_inv)) {
    return(Lambda_inv(tmp_u))
  }
  return(inverse_with_uniroot_sorted(
    f = Lambda,
    y = tmp_u,
    range_x = range_t,
    range_y = range_L
  ))
}
