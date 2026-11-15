#' Uniform random samples from `rstream` objects
#'
#' @description Sample from `rstream` objects
#' @param size Integer, number of samples
#' @param minimum Lower bound
#' @param maximum Upper bound
#' @param rng_stream (`rstream`) an `rstream` object or `NULL`
#'
#' @return a vector of uniform variates of size `size`
#' @keywords internal
#' @export
#' @importClassesFrom rstream rstream.mrg32k3a
#' @examples
#' rng_stream_runif(10)
rng_stream_runif <- function(size = 1, minimum = 0, maximum = 1, rng_stream = NULL) {
  if (!is.null(rng_stream)) {
    if (class(rng_stream)[1] == "RNGClass") {
      rng_stream <- rng_stream$.getPointer()
    }
    u <- rstream::r(rng_stream, size) * (maximum - minimum) + minimum
  } else {
    u <- stats::runif(n = size, min = minimum, max = maximum)
  }
  return(u)
}


#' Exponential random samples from `rstream` objects
#'
#' @description Sample from `rstream` objects

#' @param size Integer, number of samples
#' @param rate Positive number, the rate (i.e., 1/mean)
#' @param rng_stream (`rstream`) an `rstream` object or `NULL`
#'
#' @return a vector of exponential variates of size `size`
#' @keywords internal
#' @export
#' @importClassesFrom rstream rstream.mrg32k3a
#' @examples
#' rng_stream_rexp(10)
rng_stream_rexp <- function(size = 1, rate = 1, rng_stream = NULL) {
  if (!is.null(rng_stream)) {
    if (class(rng_stream)[1] == "RNGClass") {
      rng_stream <- rng_stream$.getPointer()
    }
    p <- rng_stream_runif(size = size, rng_stream = rng_stream)
    dt_ <- stats::qexp(p = p, rate = rate)
  } else {
    dt_ <- stats::rexp(n = size, rate = rate)
  }
  return(dt_)
}

#' Poisson random samples from `rstream` objects
#'
#' @description Sample from `rstream` objects
#' @param size Integer, number of samples
#' @param lambda Positive number, the mean
#' @param rng_stream (`rstream`) an `rstream` object or `NULL`
#'
#' @return a vector of counts of size `size`
#' @keywords internal
#' @export
#' @importClassesFrom rstream rstream.mrg32k3a
#' @examples
#' rng_stream_rpois(10)
rng_stream_rpois <- function(size = 1, lambda = 1, rng_stream = NULL) {
  if (!is.null(rng_stream)) {
    if (class(rng_stream)[1] == "RNGClass") {
      rng_stream <- rng_stream$.getPointer()
    }
    p <- rstream::r(rng_stream, size)
    x <- stats::qpois(p = p, lambda = lambda)
  } else {
    x <- stats::rpois(n = size, lambda = lambda)
  }
  return(x)
}



#' Zero-truncated Poisson random samples from `rstream` objects
#'
#' @description Sample from `rstream` objects
#' @param size Integer, number of samples
#' @param lambda Positive number, the mean of the original
#'        (untruncated) Poisson distribution
#' @param rng_stream (`rstream`) an `rstream` object or `NULL`
#'
#' @return a vector of non zero counts of size `size`
#' @keywords internal
#' @export
#' @importClassesFrom rstream rstream.mrg32k3a
#' @examples
#' rng_stream_rztpois(10)
rng_stream_rztpois <- function(size = 1, lambda = 1, rng_stream = NULL) {
  exp_minus_lambda <- exp(-lambda)
  if (!is.null(rng_stream)) {
    if (class(rng_stream)[1] == "RNGClass") {
      rng_stream <- rng_stream$.getPointer()
    }
    p <- rstream::r(rng_stream, size) * (1 - exp_minus_lambda) + exp_minus_lambda
  } else {
    p <- stats::runif(n = size, min = exp_minus_lambda, max = 1)
  }
  x <- stats::qpois(p = p, lambda = lambda)
  return(x)
}


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

#' Truncated Poisson random samples from `rstream` objects
#'
#' @description Sample from `rstream` objects a Poisson count conditional on
#' being in `[k_min, k_max]` (see `rbtpois()`).
#' @param size Integer, number of samples
#' @param lambda Positive number, the mean of the original
#'        (untruncated) Poisson distribution
#' @param k_min Non-negative integer, the minimum count conditioned on
#' @param k_max Positive integer or `Inf`, the maximum count conditioned on
#' @param rng_stream (`rstream`) an `rstream` object or `NULL`
#'
#' @return a vector of counts (all in `[k_min, k_max]`) of size `size`
#' @keywords internal
#' @export
#' @importClassesFrom rstream rstream.mrg32k3a
#' @examples
#' rng_stream_rbtpois(10, lambda = 1, k_min = 2)
rng_stream_rbtpois <- function(size = 1, lambda = 1, k_min = 1, k_max = Inf, rng_stream = NULL) {
  bounded <- is.finite(k_max)
  if (!bounded && k_min == 1) {
    return(rng_stream_rztpois(size = size, lambda = lambda, rng_stream = rng_stream))
  }
  tail_hi <- if (k_min >= 1) stats::ppois(k_min - 1, lambda = lambda, lower.tail = FALSE) else 1
  tail_lo <- if (bounded) stats::ppois(k_max, lambda = lambda, lower.tail = FALSE) else 0
  u <- rng_stream_runif(size = size, rng_stream = rng_stream)
  x <- stats::qpois(p = tail_lo + u * (tail_hi - tail_lo), lambda = lambda, lower.tail = FALSE)
  return(x)
}
