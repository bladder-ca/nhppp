#' Simulate a zero-truncated homogeneous Poisson Point Process over (t_min, t_max]
#'
#' @param t_min (scalar, double) lower bound of the time interval
#' @param t_max (scalar, double) upper bound of the time interval
#' @param rate (scalar, double) constant instantaneous rate
#' @param atmost1 boolean, draw at most 1 event time
#'
#' @return a vector of event times of size `size`
#' @export
#'
#' @examples
#' x <- ztppp(t_min = 0, t_max = 10, rate = 0.001)
ztppp <- function(rate, t_min, t_max, atmost1 = FALSE) {
  n <- rztpois(n = 1, lambda = rate * (t_max - t_min))
  tmp <- ppp_exactly_n(n = n, t_min = t_min, t_max = t_max)
  if (atmost1 == TRUE) {
    return(tmp[1])
  }
  return(tmp)
}
