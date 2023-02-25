#' Simulate from a non homogeneous Poisson Point Process (NHPPP) from
#'    (t_min, t_max) (inversion method)
#'
#' @description  Sample NHPPP times using the inversion method,
#' optionally using an `rstream` generator or a `Kystis` `RNGClass` object.
#' @param Lambda (function, double vector) a continuous increasing R to R map
#'               which is the integrated rate of the NHPPP
#' @param Lambda_inv (function, double vector) the inverse of `Lambda()`
#' @param range_t (vector, double) min and max of the time interval
#' @param range_L (vector, double) min and max of the transformed time interval
#' @param rng_stream (`rstream`) an `rstream` object.
#' @param only1 boolean, draw at most 1 event time
#'
#' @return a vector of event times (t_); if no events realize,
#'         a vector of length 0
#' @export
#'
#' @examples
#' x <- sim_nhppp_t_inv(Lambda = function(t) t + cos(t) - 1)
sim_nhppp_t_inv <- function(Lambda,
                            Lambda_inv = NULL,
                            range_t = c(0, 10),
                            range_L = c(Lambda(range_t[1]), Lambda(range_t[2])),
                            rng_stream = NULL,
                            only1 = FALSE) {
  dat_warped_time <- ppp_t(
    range_t = range_L,
    rate = 1,
    rng_stream = rng_stream,
    only1 = only1
  )

  if (length(dat_warped_time) == 0) {
    return(dat_warped_time)
  }

  if (is.function(Lambda_inv)) {
    t_ <- Lambda_inv(dat_warped_time)
  } else {
    t_ <- inverse_with_uniroot_sorted(
      f = Lambda,
      y = dat_warped_time,
      range_x = range_t,
      range_y = range_L
    )
  }
  return(t_)
}



#' Simulate from a non homogeneous Poisson Point Process (NHPPP) from
#'    (t_min, t_max) with linear intensity function (inversion method)
#'
#' @description  Sample NHPPP times from a linear intensity function
#' using the inversion method, optionally using an `rstream`
#' generator or a `Kystis` `RNGClass` object
#'
#' @param alpha (double) the intercept
#' @param beta (double) the slope
#' @param range_t (vector, double) min and max of the time interval
#' @param tol the probability that we will have more than
#'        the drawn events in (t_min, t_max]
#' @param rng_stream (`rstream`) an `rstream` object.
#' @param only1 boolean, draw at most 1 event time
#'
#' @return a vector of event times (t_); if no events realize,
#'         a vector of length 0
#' @export
#'
#' @examples
#' x <- nhppp_t_intensity_linear(alpha = 0, beta = 0.2)
#'
nhppp_t_intensity_linear <- function(alpha = 1,
                               beta = 0,
                               range_t = c(0, 10),
                               tol = 10^-6,
                               rng_stream = NULL,
                               only1 = FALSE) {
  if (beta == 0) {
    stopifnot(alpha > 0)
    return(ppp_t(range_t = range_t, rate = alpha, tol = tol, rng_stream = rng_stream, only1 = only1))
  }
  stopifnot(alpha + beta * range_t[1] >= 0)
  if (beta < 0) {
    t_max <- -alpha / beta
    range_t[2] <- min(range_t[2], t_max)
  }
  return(
    sim_nhppp_t_inv(
      Lambda = function(t) Lambda_linear_form(t, alpha = alpha, beta = beta, t0 = range_t[1]),
      Lambda_inv = function(z) Lambda_inv_linear_form(z, alpha = alpha, beta = beta, t0 = range_t[1]),
      range_t = range_t,
      rng_stream = rng_stream,
      only1 = only1
    )
  )
}
