#' Generic function for simulating from NHPPPs given the intensity function
#' or the cumulative intensity function.
#'
#' This is a wrapper to the package's specific functions, and thus somewhat slower.
#' For time-intensive simulations prefer one of the specific functions.
#'
#' @param lambda (function) the instantaneous rate of the NHPPP.
#' A continuous function of time.
#' @param lambda_maj (double, vector) the intercept and optional slope of the majorizing
#' linear (if `exp_maj = FALSE`) or log-linear (if `exp_maj = TRUE`) function in `range_t`.
#' @param Lambda (function, double vector) a continuous increasing R to R map
#'               which is the integrated rate of the NHPPP
#' @param Lambda_inv (function, double vector) the inverse of `Lambda()`
#' @param range_t (vector, double) min and max of the time interval.
#' @param rng_stream (`rstream`) an `rstream` object or `NULL`
#' @param atmost1 boolean, draw at most 1 event time
#' @param atleast1 boolean, draw at least 1 event time in interval
#'
#' @return a vector of event times
#' @export
draw <- function(
    lambda = NULL,
    lambda_maj = NULL,
    Lambda = NULL,
    Lambda_inv = NULL,
    range_t = c(0, 10),
    rng_stream = NULL,
    atmost1 = FALSE,
    atleast1 = FALSE) {
  if (!is.null(lambda)) {
    if (atleast1) {
      func <- ztdraw_intensity
    } else {
      func <- draw_intensity
    }
    return(func(
      lambda = lambda,
      lambda_maj = lambda_maj,
      exp_maj = FALSE,
      range_t = range_t,
      rng_stream = rng_stream,
      atmost1 = atmost1
    ))
  }

  if (!is.null(Lambda)) {
    if (atleast1) {
      func <- ztdraw_cumulative_intensity
    } else {
      func <- draw_cumulative_intensity_inversion
    }
    return(func(
      Lambda = Lambda,
      Lambda_inv = Lambda_inv,
      range_t = range_t,
      range_L = c(Lambda(range_t[1]), Lambda(range_t[2])),
      rng_stream = rng_stream,
      atmost1 = atmost1
    ))
  }
}
