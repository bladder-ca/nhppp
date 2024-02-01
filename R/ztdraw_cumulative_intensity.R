#' Simulate from a zero-truncated non homogeneous Poisson Point Process (zt-NHPPP) from
#'    (t_min, t_max) (order statistics method)
#'
#' @description  Sample zero-truncated NHPPP times using the order statistics method,
#' optionally using an `rstream` generator
#' @param Lambda (function, double vector) a continuous increasing R to R map
#'               which is the integrated rate of the NHPPP
#' @param Lambda_inv (function, double vector) the inverse of `Lambda()`
#' @param range_t (vector, double) min and max of the time interval
#' @param range_L (vector, double) min and max of the transformed time interval
#' @param rng_stream (`rstream`) an `rstream` object or `NULL`.
#' @param atmost1 (boolean) draw at most 1 event time
#'
#' @return a vector of at least 1 event times
#' @export
#'
#' @examples
#' x <- ztdraw_cumulative_intensity(Lambda = function(t) t + cos(t) - 1)
ztdraw_cumulative_intensity <- function(Lambda,
                                        Lambda_inv = NULL,
                                        range_t = c(0, 10),
                                        range_L = c(Lambda(range_t[1]), Lambda(range_t[2])),
                                        rng_stream = NULL,
                                        atmost1 = FALSE) {
  tmp_u <- ztppp(range_t = range_L, rate = 1, rng_stream = rng_stream)
  if (atmost1 == TRUE) {
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
