#' Simulate a homogeneous Poisson Point Process over (t_min, t_max] (order statistics method)
#'
#' Internal function -- not to be exported.
#' Same as `ppp` but uses the Order Statistics algorithm.
#' @param rate (scalar, double) constant instantaneous rate
#' @param t_min (scalar, double) the lower bound of the time interval
#' @param t_max (scalar, double) the upper bound of the time interval
#' @param atmost1 boolean, draw at most 1 event time
#'
#' @return a vector of event times t
#'         if no events realize, it will have 0 length
#'
#' @keywords internal
#' @examples
#' x <- ppp(rate = 1, t_min = 0, t_max = 10, tol = 10^-6)
ppp2 <- function(rate, t_min, t_max, atmost1 = FALSE) {
  n <- stats::rpois(n = 1, lambda = rate * (t_max - t_min))
  tmp <- ppp_exactly_n(n = n, t_min = t_min, t_max = t_max)
  if (atmost1 == TRUE) {
    return(tmp[1])
  }
  return(tmp)
}
