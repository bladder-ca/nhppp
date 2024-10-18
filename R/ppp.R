#' Simulate a homogeneous Poisson Point Process in (t_min, t_max]
#'
#' @param rate (scalar, double) constant instantaneous rate
#' @param t_min (scalar, double) the lower bound of the time interval
#' @param t_max (scalar, double) the upper bound of the time interval
#' @param atmost1 boolean, draw at most 1 event time
#' @param tol the probability that we will have more than
#'        the drawn events in (t_min, t_max]
#'
#' @return a vector of event times t
#'         if no events realize, it will have 0 length
#' @export
#'
#' @examples
#' x <- ppp(rate = 1, t_min = 0, t_max = 10, tol = 10^-6)
ppp <- function(rate, t_min, t_max, atmost1 = FALSE, tol = 10^-6) {
  if (isTRUE(atmost1)) {
    n <- 1
  } else {
    n <- stats::qpois(p = 1 - tol, lambda = rate * (t_max - t_min))
  }
  dt_ <- stats::rexp(n = n, rate = rate)
  t_ <- cumsum(dt_) + t_min
  return(t_[t_ <= t_max])
}
