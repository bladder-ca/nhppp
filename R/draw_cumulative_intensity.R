#' Simulate from a non homogeneous Poisson Point Process (NHPPP)
#'  over an interval when you know the cumulative intensity and its inverse.
#'
#' @description  Sample NHPPP times using the inversion method
#' @param Lambda (function, double vector) a continuous increasing R to R map
#'               which is the integrated rate of the NHPPP
#' @param Lambda_inv (function, double vector) the inverse of `Lambda()`
#' @param t_min (double) the lower bound of the time interval
#' @param t_max (double) the upper bound of the time interval
#' @param atmost1 boolean, draw at most 1 event time
#'
#' @return a vector of event times (t_); if no events realize,
#'         a vector of length 0
#' @export
draw_cumulative_intensity <- function(Lambda,
                                      Lambda_inv,
                                      t_min,
                                      t_max,
                                      atmost1 = FALSE) {
  draw_cumulative_intensity_inversion(
    Lambda = Lambda,
    Lambda_inv = Lambda_inv,
    t_min = t_min,
    t_max = t_max,
    atmost1 = atmost1
  )
}
