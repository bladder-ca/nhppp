#' Simulate from a non homogeneous Poisson Point Process (NHPPP)
#'  over an interval when you know the cumulative intensity and its inverse.
#'
#' @description  Sample NHPPP times using the inversion method
#' @param Lambda (function, double vector) a continuous increasing R to R map
#'               which is the integrated rate of the NHPPP
#' @param Lambda_inv (function, double vector) the inverse of `Lambda()`
#' @param t_min (double) the lower bound of the time interval
#' @param t_max (double) the upper bound of the time interval
#' @param atmost1 boolean, report at most 1 event time (alias for
#'        `report_first_K = 1`)
#' @inheritParams draw_sc_step
#'
#' @return a vector of event times (t_); if no events realize,
#'         a vector of length 0
#' @export
draw_cumulative_intensity <- function(Lambda,
                                      Lambda_inv,
                                      t_min,
                                      t_max,
                                      atmost1 = FALSE,
                                      report_first_K = NULL,
                                      report_last_K = NULL,
                                      atleast1 = FALSE,
                                      generate_at_least_K = NULL,
                                      generate_at_most_K = NULL) {
  rep_ <- .resolve_reporting(atmost1, report_first_K, report_last_K)
  gen_ <- .resolve_generation(atleast1, generate_at_least_K, generate_at_most_K)
  return(.run_scalar_sampler(
    rep_, gen_,
    plain_fun = function(atmost1) {
      draw_cumulative_intensity_inversion(
        Lambda = Lambda,
        Lambda_inv = Lambda_inv,
        t_min = t_min,
        t_max = t_max,
        atmost1 = atmost1
      )
    },
    zt_fun = function(...) {
      ztdraw_cumulative_intensity(Lambda = Lambda, Lambda_inv = Lambda_inv, t_min = t_min, t_max = t_max, ...)
    }
  ))
}
