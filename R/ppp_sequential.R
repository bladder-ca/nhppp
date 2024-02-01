#' Simulate a homogeneous Poisson Point Process over (t_min, t_max]
#'
#' @param range_t (vector, double) min and max of the time interval
#' @param rate (scalar, double) constant instantaneous rate
#' @param tol the probability that we will have more than
#'        the drawn events in (t_min, t_max]
#' @param rng_stream an `rstream` object
#' @param atmost1 boolean, draw at most 1 event time
#'
#' @return a vector of event times t
#'         if no events realize, it will have 0 length
#' @export
#'
#' @examples
#' x <- ppp_sequential(range_t = c(0, 10), rate = 1, tol = 10^-6)
ppp_sequential <- function(range_t = c(0, 10), rate = 1, tol = 10^-6, rng_stream = NULL, atmost1 = FALSE) {
  # we expect lambda = t_max*rate events
  # we will draw n so that the probability that t>t_max is 1-tol
  if (isTRUE(atmost1)) {
    n <- 1
  } else {
    n <- stats::qpois(p = 1 - tol, lambda = rate * (range_t[2] - range_t[1]))
  }
  dt_ <- rng_stream_rexp(size = n, rate = rate, rng_stream = rng_stream)
  t_ <- cumsum(dt_) + range_t[1]
  return(t_[t_ <= range_t[2]])
}
