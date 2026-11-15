#' Special case: Simulate from a non homogeneous Poisson Point Process (NHPPP) from
#'    (t_min, t_max) with log-linear intensity function (inversion method)
#'
#' @description  Sample NHPPP times from an log linear intensity function
#' using the inversion method, optionally using an `rstream`
#' generator
#'
#' @param intercept (double) the intercept in the exponent
#' @param slope (double) the slope in the exponent
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
#' x <- draw_sc_loglinear(intercept = 0, slope = 0.2, t_min = 0, t_max = 10)
#'
draw_sc_loglinear <- function(intercept,
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
    if (slope == 0) {
      return(ppp2(rate = exp(intercept), t_min = t_min, t_max = t_max, atmost1 = atmost1))
    }
    return(
      draw_cumulative_intensity_inversion(
        Lambda = function(t) Lambda_exp_form(t, intercept = intercept, slope = slope, t0 = t_min),
        Lambda_inv = function(z) Lambda_inv_exp_form(z, intercept = intercept, slope = slope, t0 = t_min),
        t_min = t_min,
        t_max = t_max,
        atmost1 = atmost1
      )
    )
  }
  return(.run_scalar_sampler(
    rep_, gen_,
    plain_fun = plain_fun,
    zt_fun = function(...) ztdraw_sc_loglinear(intercept = intercept, slope = slope, t_min = t_min, t_max = t_max, ...)
  ))
}
