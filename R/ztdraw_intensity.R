#' Simulate `size` samples from a zero-truncated non homogeneous Poisson Point Process (zt-NHPPP) from
#'    (t0, t_max) (thinning method)
#'
#' @description Sample zero-truncated NHPPP intensity times using the thinning method, optionally using
#' an `rstream` generator
#'
#' @param lambda (function) the instantaneous rate of the NHPPP.
#' A continuous function of time.
#' @param lambda_maj (double, vector) the intercept and optional slope of the majorizing
#' linear (if `exp_maj = FALSE`) or log-linear (if `exp_maj = TRUE`) function in `range_t`.
#' @param exp_maj (boolean) if `TRUE` the majorizer is `exp(alpha + beta * t)`
#' @param range_t (vector, double) min and max of the time interval.
#' @param rng_stream (`rstream`) an `rstream` object or `NULL`
#' @param atmost1 (boolean) draw at most 1 event time
#'
#' @return a vector of at least 1 event times
#' @export
#'
#' @examples
#' x <- ztdraw_intensity(lambda = function(t) 1 + sin(t))
ztdraw_intensity <- function(lambda,
                             lambda_maj = NULL,
                             exp_maj = FALSE,
                             range_t = c(0, 10),
                             rng_stream = NULL,
                             atmost1 = FALSE) {
  if (is.null(lambda_maj)) {
    alpha <- stats::optimize(
      f = function(x) lambda(x),
      interval = range_t,
      maximum = TRUE
    )$objective * 1.2
    beta <- 0
  } else if (length(lambda_maj) == 1) {
    alpha <- lambda_maj[1]
    beta <- 0
  } else if (length(lambda_maj) == 2) {
    alpha <- lambda_maj[1]
    beta <- lambda_maj[2]
  }

  if (isTRUE(exp_maj)) {
    ztnhppp_t <- ztdraw_sc_loglinear
    link <- exp
  } else {
    ztnhppp_t <- ztdraw_sc_linear
    link <- identity
  }

  while (TRUE) {
    candidate_times <- ztnhppp_t(alpha = alpha, beta = beta, range_t = range_t, rng_stream = rng_stream, atmost1 = FALSE)
    u <- rng_stream_runif(size = length(candidate_times), minimum = 0, maximum = 1, rng_stream = rng_stream)
    acceptance_prob <- lambda(candidate_times) / link(alpha + beta * candidate_times)
    if (!all(acceptance_prob <= 1 + 10^-6)) stop("lambda > lambda_maj\n")
    candidate_times <- candidate_times[u < acceptance_prob]
    if (length(candidate_times) > 0) {
      if (atmost1) {
        return(candidate_times[1])
      } else {
        return(candidate_times)
      }
    }
  }
}
