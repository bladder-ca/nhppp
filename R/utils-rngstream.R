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

#' K-truncated Poisson random samples (basic R)
#'
#' @description Sample from a Poisson distribution conditional on the count
#' being at least `k`, by inversion of the CDF restricted to the truncation
#' region. `k = 1` is the zero-truncated distribution (`rztpois()`); `k >= 2`
#' samples the upper tail directly (`p ~ U(0, P(N >= k))` with the upper-tail
#' quantile), which stays accurate when `P(N >= k)` is tiny.
#'
#' @param n Integer, number of samples
#' @param lambda Positive number (scalar or vector), the mean of the original
#'        (untruncated) Poisson distribution
#' @param k Positive integer, the minimum count conditioned on
#'
#' @return a vector of counts (all `>= k`) of size `n`
#' @export
#' @keywords internal
#' @examples
#' rbtpois(10, 1, k = 3)
rbtpois <- function(n, lambda, k = 1) {
  if (k <= 1) {
    return(rztpois(n = n, lambda = lambda))
  }
  tail_mass <- stats::ppois(k - 1, lambda = lambda, lower.tail = FALSE) # P(N >= k)
  p <- stats::runif(n = n) * tail_mass
  x <- stats::qpois(p = p, lambda = lambda, lower.tail = FALSE)
  return(x)
}

#' K-truncated Poisson random samples from `rstream` objects
#'
#' @description Sample from `rstream` objects a Poisson count conditional on
#' being at least `k` (see `rbtpois()`).
#' @param size Integer, number of samples
#' @param lambda Positive number, the mean of the original
#'        (untruncated) Poisson distribution
#' @param k Positive integer, the minimum count conditioned on
#' @param rng_stream (`rstream`) an `rstream` object or `NULL`
#'
#' @return a vector of counts (all `>= k`) of size `size`
#' @keywords internal
#' @export
#' @importClassesFrom rstream rstream.mrg32k3a
#' @examples
#' rng_stream_rbtpois(10, lambda = 1, k = 2)
rng_stream_rbtpois <- function(size = 1, lambda = 1, k = 1, rng_stream = NULL) {
  if (k <= 1) {
    return(rng_stream_rztpois(size = size, lambda = lambda, rng_stream = rng_stream))
  }
  tail_mass <- stats::ppois(k - 1, lambda = lambda, lower.tail = FALSE) # P(N >= k)
  u <- rng_stream_runif(size = size, rng_stream = rng_stream)
  x <- stats::qpois(p = u * tail_mass, lambda = lambda, lower.tail = FALSE)
  return(x)
}
