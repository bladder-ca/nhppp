#' Definite integral of `l = alpha + beta*t` at time `t`
#' with `L(t0) = 0`
#'
#' @description Definite integral of `l = alpha + beta*t` starting at
#'              `t0` -- only for `l+`.
#' @param t (double) the time point
#' @param alpha (double) the intercept
#' @param beta (double) the slope
#' @param t0 (double) the starting time
Lambda_linear_form <- function(t, alpha, beta, t0) {
  stopifnot(min(t) >= t0)
  if (beta < 0) {
    t[t > -alpha / beta] <- -alpha / beta
  }
  return(alpha * (t - t0) + (beta / 2) * (t^2 - t0^2))
}

#' Inverse of the definite integral of `l = alpha + beta*t` at time `t`
#'
#' @description Inverse of the definite integral of `l = alpha + beta*t` only for `l+`.
#' @param z (double) the value of integrated rate for which you want to find the time
#' @param alpha (double) the intercept
#' @param beta (double) the slope
#' @param t0 (double) the starting time
Lambda_inv_linear_form <- function(z, alpha, beta, t0) {
  stopifnot(beta != 0 || alpha != 0)
  if (beta != 0) {
    L0 <- -alpha * t0 - beta / 2 * t0^2
    Delta <- alpha^2 - 2 * beta * (L0 - z)
    stopifnot(all(Delta >= 0))
    t_ <- (-alpha + sqrt(Delta)) / beta
  } else if (beta == 0) {
    t_ <- z / alpha + t0
  }
  return(t_)
}



#' Definite integral of `l = exp(alpha + beta*t)` at time `t`
#' with `L(t0) = 0`
#'
#' @description Definite integral of `l = exp(alpha + beta*t)` starting at
#'              `t0` -- only for `l+`.
#' @param t (double) the time point
#' @param alpha (double) the intercept
#' @param beta (double) the slope
#' @param t0 (double) the starting time
Lambda_exp_form <- function(t, alpha, beta, t0) {
  stopifnot(min(t) >= t0)
  return( (exp(beta*t + alpha) - exp(beta*t0 + alpha))/beta)
}

#' Inverse of the definite integral of `l = exp(alpha + beta*t)` at time `t`
#'
#' @description Inverse of the definite integral of `l = exp(alpha + beta*t)` only for `l+`.
#' @param z (double) the value of integrated rate for which you want to find the time
#' @param alpha (double) the intercept
#' @param beta (double) the slope
#' @param t0 (double) the starting time
Lambda_inv_exp_form <- function(z, alpha, beta, t0) {
  tmp <- exp(beta*t0+alpha)
  #stopifnot(beta > - tmp / z && beta != 0)
  return((log(tmp + z*beta) - alpha)/beta)
}


