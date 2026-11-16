#' Special case: Simulate from a non homogeneous Poisson Point Process (NHPPP) from
#'    (t_min, t_max) with linear intensity function (inversion method)
#'
#' @description  Sample NHPPP times from a linear intensity function
#' using the inversion method.
#'
#' @param intercept (double) the intercept
#' @param slope (double) the slope
#' @param t_min (double) lower bound of the time interval
#' @param t_max (double) upper bound of the time interval
#' @param atmost1 boolean, report at most 1 event time (alias for
#'        `report_first_K = 1`)
#' @inheritParams draw_sc_step
#'
#' @return a vector of event times (t_); if no events realize,
#'         a vector of length 0
#' @export
#'
#' @examples
#' x <- draw_sc_linear(intercept = 0, slope = 0.2, t_min = 0, t_max = 10)
#'
draw_sc_linear <- function(intercept,
                           slope,
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
  plain_fun <- function(atmost1) {
    if ((slope <= 0 && intercept <= 0) || (intercept + slope * t_min < 0)) {
      return(c())
    }
    if (slope == 0) {
      return(ppp2(rate = intercept, t_min = t_min, t_max = t_max, atmost1 = atmost1))
    }
    if (slope < 0) {
      t_upper <- -intercept / slope
      t_max <- min(t_upper, t_max)
    }
    return(
      draw_cumulative_intensity_inversion(
        Lambda = function(t) Lambda_linear_form(t, intercept = intercept, slope = slope, t0 = t_min),
        Lambda_inv = function(z) Lambda_inv_linear_form(z, intercept = intercept, slope = slope, t0 = t_min),
        t_min = t_min,
        t_max = t_max,
        atmost1 = atmost1
      )
    )
  }
  return(.run_scalar_sampler(
    rep_, gen_,
    plain_fun = plain_fun,
    zt_fun = function(...) ztdraw_sc_linear(intercept = intercept, slope = slope, t_min = t_min, t_max = t_max, ...)
  ))
}
