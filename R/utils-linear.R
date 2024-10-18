#' Definite integral of `l = intercept + slope*t` at time `t`
#' with `L(t0) = 0`
#'
#' @description Definite integral of `l = intercept + slope*t` starting at
#'              `t0` -- only for `l+`.
#' @param t (double) the time point
#' @param intercept (double) the intercept
#' @param slope (double) the slope
#' @param t0 (double) the starting time
#' @keywords internal
Lambda_linear_form <- function(t, intercept, slope, t0) {
  if (min(t) < t0) stop()
  if (slope < 0) {
    t[t > -intercept / slope] <- -intercept / slope
  }
  return(intercept * (t - t0) + (slope / 2) * (t^2 - t0^2))
}

#' Inverse of the definite integral of `l = intercept + slope*t` at time `t`
#'
#' @description Inverse of the definite integral of `l = intercept + slope*t` only for `l+`.
#' @param z (double) the value of integrated rate for which you want to find the time
#' @param intercept (double) the intercept
#' @param slope (double) the slope
#' @param t0 (double) the starting time
#' @keywords internal
Lambda_inv_linear_form <- function(z, intercept, slope, t0) {
  if (slope == 0 & intercept == 0) stop()
  if (slope != 0) {
    L0 <- -intercept * t0 - slope / 2 * t0^2
    Delta <- intercept^2 - 2 * slope * (L0 - z)
    if (!all(Delta >= 0)) stop()
    t_ <- (-intercept + sqrt(Delta)) / slope
  } else if (slope == 0) {
    t_ <- z / intercept + t0
  }
  return(t_)
}

#' Definite integral of `l = exp(intercept + slope*t)` at time `t`
#' with `L(t0) = 0`
#'
#' @description Definite integral of `l = exp(intercept + slope*t)` starting at
#'              `t0` -- only for `l+`.
#' @param t (double) the time point
#' @param intercept (double) the intercept
#' @param slope (double) the slope
#' @param t0 (double) the starting time
#' @keywords internal
Lambda_exp_form <- function(t, intercept, slope, t0) {
  if (min(t) < t0) stop()
  return((exp(slope * t + intercept) - exp(slope * t0 + intercept)) / slope)
}

#' Inverse of the definite integral of `l = exp(intercept + slope*t)` at time `t`
#'
#' @description Inverse of the definite integral of `l = exp(intercept + slope*t)` only for `l+`.
#' @param z (double) the value of integrated rate for which you want to find the time
#' @param intercept (double) the intercept
#' @param slope (double) the slope
#' @param t0 (double) the starting time
#' @keywords internal
Lambda_inv_exp_form <- function(z, intercept, slope, t0) {
  tmp <- exp(slope * t0 + intercept)
  return((log(tmp + z * slope) - intercept) / slope)
}
