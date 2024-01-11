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
  return((exp(beta * t + alpha) - exp(beta * t0 + alpha)) / beta)
}

#' Inverse of the definite integral of `l = exp(alpha + beta*t)` at time `t`
#'
#' @description Inverse of the definite integral of `l = exp(alpha + beta*t)` only for `l+`.
#' @param z (double) the value of integrated rate for which you want to find the time
#' @param alpha (double) the intercept
#' @param beta (double) the slope
#' @param t0 (double) the starting time
Lambda_inv_exp_form <- function(z, alpha, beta, t0) {
  tmp <- exp(beta * t0 + alpha)
  # stopifnot(beta > - tmp / z && beta != 0)
  return((log(tmp + z * beta) - alpha) / beta)
}

#' Piecewise linear majorizer for K-Lipschitz functions over an interval 
#' 
#' @description Return a piecewise linear majorizer for K-Lipschitz functions
#'              over an interval. 
#' @param fun A function object with a single argument `x`
#' @param breaks (vector) The set of `M+1` boundaries for the `M` subintervals in `x`
#' @param is_monotonic (boolean) Is the function monotonic? (Default is `TRUE`.)
#' @param K (double) A non-negative number for the Lipschitz cone. (Default is 0.) 
#' @export
get_piecewise_linear_majorizer <- function(fun, breaks, is_monotonic = TRUE, K = 0) {
  stopifnot(K >= 0)
  M <- length(breaks) - 1 
  f_breaks <- fun(breaks)
  lambda_star <- pmax(f_breaks[1:M], f_breaks[2:(M+1)])
  if(isTRUE(is_monotonic)) return(lambda_star)  
  else return(lambda_star + K * abs(breaks[1:M] - breaks[2:(M+1)])/2)
}
