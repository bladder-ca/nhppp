#' Zero-truncated Poisson random samples (basic R)
#'
#' @description Sample zero-truncated Poisson random samples (basic R)
#' @param n Integer, number of samples
#' @param lambda Positive number, the mean of the original
#'        (untruncated) Poisson distribution
#'
#' @return a vector of non zero counts of size `n`
#' @export
#' @keywords internal
#' @examples
#' rztpois(10, 1)
#' rztpois(10, 1:10)
rztpois <- function(n, lambda) {
  p <- stats::runif(n = n, min = exp(-lambda), max = 1)
  x <- stats::qpois(p = p, lambda = lambda)
  return(x)
}

#' Truncated Poisson random samples (basic R)
#'
#' @description Sample from a Poisson distribution conditional on the count
#' being in `[k_min, k_max]`, by inversion of the CDF restricted to the
#' truncation region (upper-tail parameterization, accurate when the region
#' is deep in the right tail). `k_min = 1, k_max = Inf` is the zero-truncated
#' distribution (`rztpois()`).
#'
#' @param n Integer, number of samples
#' @param lambda Positive number (scalar or vector), the mean of the original
#'        (untruncated) Poisson distribution
#' @param k_min Non-negative integer, the minimum count conditioned on
#' @param k_max Positive integer or `Inf`, the maximum count conditioned on
#'
#' @return a vector of counts (all in `[k_min, k_max]`) of size `n`
#' @export
#' @keywords internal
#' @examples
#' rbtpois(10, 1, k_min = 3)
#' rbtpois(10, 1, k_min = 0, k_max = 2)
rbtpois <- function(n, lambda, k_min = 1, k_max = Inf) {
  bounded <- is.finite(k_max)
  if (!bounded && k_min == 1) {
    return(rztpois(n = n, lambda = lambda))
  }
  tail_hi <- if (k_min >= 1) stats::ppois(k_min - 1, lambda = lambda, lower.tail = FALSE) else 1 # P(N >= k_min)
  tail_lo <- if (bounded) stats::ppois(k_max, lambda = lambda, lower.tail = FALSE) else 0 # P(N > k_max)
  p <- tail_lo + stats::runif(n = n) * (tail_hi - tail_lo)
  x <- stats::qpois(p = p, lambda = lambda, lower.tail = FALSE)
  return(x)
}
