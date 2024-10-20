#' Simulate from a non homogeneous Poisson Point Process (NHPPP) from
#'    (t_min, t_max) (inversion method)
#'
#' @description  Sample NHPPP times using the inversion method,
#' optionally using an `rstream` generator object
#' @param Lambda (function, double vector) a continuous increasing R to R map
#'               which is the integrated rate of the NHPPP
#' @param Lambda_inv (function, double vector) the inverse of `Lambda()`
#' @param t_min (double) the lower bound of the time interval
#' @param t_max (double) the upper bound of the time interval
#' @param atmost1 boolean, draw at most 1 event time
#'
#' @return a vector of event times (t_); if no events realize,
#'         a vector of length 0
#' @keywords internal
draw_cumulative_intensity_inversion <- function(Lambda,
                                                Lambda_inv,
                                                t_min,
                                                t_max,
                                                atmost1 = FALSE) {
  L_min <- Lambda(t_min)
  L_max <- Lambda(t_max)
  dat_warped_time <- ppp(
    rate = 1,
    t_min = L_min,
    t_max = L_max,
    atmost1 = atmost1
  )

  if (length(dat_warped_time) == 0) {
    return(dat_warped_time)
  }
  return(Lambda_inv(dat_warped_time))
}
