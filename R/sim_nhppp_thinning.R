#' Simulate from a non homogeneous Poisson Point Process (NHPPP) from
#'    (t_min, t_max) (thinning method)
#'
#' @description Sample NHPPP times using the thinning method, optionally using
#' an `rstream` generator or a `Kystis` `RNGClass` object.
#' @param lambda (function) the instantaneous rate of the NHPPP.
#' A continuous function of time.
#' @param lambda_max (double) the maximum of `lambda(t)` in `range_t`.
#' @param range_t (vector, double) min and max of the time interval.
#' @param rng_stream (`rstream`) an `rstream` or `RNGClass` object, or `NULL`
#' @param only1 boolean, draw at most 1 event time
#'
#' @return a vector of event times (t_); if no events realize,
#'         a vector of length 0
#' @export
#'
#' @examples
#' x <- sim_nhppp_t_thinning(lambda = function(t) 1 + sin(t))
sim_nhppp_t_thinning <- function(lambda,
                                 lambda_max = NULL,
                                 range_t = c(0, 10),
                                 rng_stream = NULL,
                                 only1 = FALSE) {
  if (is.null(lambda_max)) {
    lambda_max <- stats::optimize(
      f = function(x) lambda(x),
      interval = range_t,
      maximum = TRUE
    )$objective
  }
  lambda_star <- lambda_max
  X <- numeric()
  t_min <- range_t[1]
  while (t_min <= range_t[2]) {
    # draw 2 uniforms per while; second is used in the thinning
    u <- rng_stream_runif(size = 2, minimum = 0, maximum = 1, rng_stream = rng_stream)
    t_min <- t_min - log(u[1]) / lambda_star
    # the last t_min could go above range_t[2] - catch it here
    if (u[2] < lambda(t_min) / lambda_star &&
      t_min <= range_t[2]) {
      X <- c(X, t_min)
    }
    if (isTRUE(only1) && length(X) == 1) {
      return(X)
    }
  }
  return(X)
}
