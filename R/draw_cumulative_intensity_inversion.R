#' Simulate from a non homogeneous Poisson Point Process (NHPPP) from
#'    (t_min, t_max) (inversion method)
#'
#' @description  Sample NHPPP times using the inversion method,
#' optionally using an `rstream` generator object
#' @param Lambda (function, double vector) a continuous increasing R to R map
#'               which is the integrated rate of the NHPPP
#' @param Lambda_inv (function, double vector) the inverse of `Lambda()`
#' @param range_t (vector, double) min and max of the time interval
#' @param range_L (vector, double) min and max of the transformed time interval
#' @param rng_stream (`rstream`) an `rstream` object.
#' @param atmost1 boolean, draw at most 1 event time
#'
#' @return a vector of event times (t_); if no events realize,
#'         a vector of length 0
#' @export
#'
#' @examples
#' x <- draw_cumulative_intensity_inversion(Lambda = function(t) t + cos(t) - 1)
draw_cumulative_intensity_inversion <- function(Lambda,
                                                Lambda_inv = NULL,
                                                range_t = c(0, 10),
                                                range_L = c(Lambda(range_t[1]), Lambda(range_t[2])),
                                                rng_stream = NULL,
                                                atmost1 = FALSE) {
  dat_warped_time <- ppp_sequential(
    range_t = range_L,
    rate = 1,
    rng_stream = rng_stream,
    atmost1 = atmost1
  )

  if (length(dat_warped_time) == 0) {
    return(dat_warped_time)
  }
  if (is.function(Lambda_inv)) {
    return(Lambda_inv(dat_warped_time))
  }
  return(inverse_with_uniroot_sorted(
    f = Lambda,
    y = dat_warped_time,
    range_x = range_t,
    range_y = range_L
  ))
}
