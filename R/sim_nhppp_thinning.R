#' Simulate from a non homogeneous Poisson Point Process (NHPPP) from
#'    (t0, t_max) (thinning method)
#'
#' @description Sample NHPPP times using the thinning method, optionally using
#' an `rstream` generator or a `Kystis` `RNGClass` object.
#' @param lambda (function) the instantaneous rate of the NHPPP.
#' A continuous function of time.
#' @param lambda_maj (double, vector) the intercept and optional slope of the majorizing
#' linear function in `range_t`.
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
                                 lambda_maj = NULL,
                                 range_t = c(0, 10),
                                 rng_stream = NULL,
                                 only1 = FALSE) {
  # browser()
  if (is.null(lambda_maj)) {
    alpha <- stats::optimize(
      f = function(x) lambda(x),
      interval = range_t,
      maximum = TRUE
    )$objective
    beta <- 0
  } else if (length(lambda_maj) == 1) {
    alpha <- lambda_maj[1]
    beta <- 0
  } else if (length(lambda_maj) == 2) {
    alpha <- lambda_maj[1]
    beta <- lambda_maj[2]
  }

  X <- numeric()
  t0 <- range_t[1]

  while (t0 <= range_t[2]) {
    t0 <- sim_nhppp_t_linear(alpha = alpha, beta = beta, range_t = c(t0, range_t[2]), rng_stream = rng_stream, only1 = TRUE)
    if (length(t0) == 0) {
      break
    }
    u <- rng_stream_runif(size = 1, minimum = 0, maximum = 1, rng_stream = rng_stream)
    # the last t0 could go above range_t[2] - catch it here
    acceptance_prob <- lambda(t0) / (alpha + beta * t0)

    if (u < acceptance_prob && t0 <= range_t[2]) {
      stopifnot(acceptance_prob >= 0 && acceptance_prob <= 1)
      X <- c(X, t0)
    }
    if (isTRUE(only1) && length(X) == 1) {
      return(X)
    }
  }
  return(X)
}
