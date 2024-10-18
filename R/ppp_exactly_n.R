#' Simulate exactly `n` points from a homogeneous Poisson Point Process over (t_min, t_max]
#'
#' @param n (int) the number of points to be simulated
#' @param t_min (double) the lower bound of the time interval
#' @param t_max (double) the upper bound of the time interval
#'
#' @return a vector of event times of size `n`
#' @export
#'
#' @examples
#' x <- ppp_exactly_n(n = 10, t_min = 0, t_max = 10)
ppp_exactly_n <- function(n, t_min, t_max) {
  U <- stats::runif(n = n, min = t_min, max = t_max)
  if (n == 1) {
    return(U)
  }
  return(U[order(U, method = "shell")])
}
