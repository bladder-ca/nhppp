#' Simulate from a zero-truncated non homogeneous Poisson Point Process (zt-NHPPP) from
#'    (t_min, t_max) (order statistics method)
#'
#' @description  Sample zero-truncated NHPPP times using the order statistics method,
#' optionally using an `rstream` generator or a `Kystis` `RNGClass` object.
#' @param Lambda (function, double vector) a continuous increasing R to R map
#'               which is the integrated rate of the NHPPP
#' @param Lambda_inv (function, double vector) the inverse of `Lambda()`
#' @param range_t (vector, double) min and max of the time interval
#' @param range_L (vector, double) min and max of the transformed time interval
#' @param rng_stream (`rstream`) an `rstream`, `RNGClass` object or `NULL`.
#' @param only1 (boolean) draw at most 1 event time
#'
#' @return a vector of event times (t_); if no events realize,
#'         a vector of length 0
#' @export
#'
#' @examples
#' x <- ztnhppp_t_cumulative_intensity_orderstats(Lambda = function(t) t + cos(t) - 1)
ztnhppp_t_cumulative_intensity <- function(Lambda,
                                           Lambda_inv = NULL,
                                           range_t = c(0, 10),
                                           range_L = c(Lambda(range_t[1]), Lambda(range_t[2])),
                                           rng_stream = NULL,
                                           only1 = FALSE) {
  tmp_u <- ztppp_t(range_t = range_L, rate = 1, rng_stream = rng_stream)
  if (only1 == TRUE) {
    tmp_u <- tmp_u[1]
  }

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
