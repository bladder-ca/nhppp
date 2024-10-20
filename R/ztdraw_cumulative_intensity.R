#' Simulate from a zero-truncated non homogeneous Poisson Point Process (zt-NHPPP) from
#'    (t_min, t_max) (order statistics method)
#'
#' @description  Sample zero-truncated NHPPP times using the order statistics method,
#' optionally using an `rstream` generator
#' @param Lambda (function, double vector) a continuous increasing R to R map
#'               which is the integrated rate of the NHPPP
#' @param Lambda_inv (function, double vector) the inverse of `Lambda()`
#' @param t_min (double) the lower bound of the time interval
#' @param t_max (double) the upper bound of the time interval
#' @param atmost1 (boolean) draw at most 1 event time
#'
#' @return a vector of at least 1 event times
#' @export
ztdraw_cumulative_intensity <- function(Lambda,
                                        Lambda_inv,
                                        t_min,
                                        t_max,
                                        atmost1 = FALSE) {
  tmp_u <- ztppp(rate = 1, t_min = Lambda(t_min), t_max = Lambda(t_max))
  if (atmost1 == TRUE) {
    tmp_u <- tmp_u[1]
  }
  return(Lambda_inv(tmp_u))
}
