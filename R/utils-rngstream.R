#' Uniform random samples from `rstream` or `RGNClass` objects
#'
#' @description Sample from `rstream` or `RGNClass` objects
#' @param size Integer, number of samples
#' @param minimum Lower bound
#' @param maximum Upper bound
#' @param rng_stream (`rstream`) an `rstream` object or `NULL`
#'
#' @return a vector of uniform variates of size `size`
#' @export
#' @importClassesFrom rstream rstream.mrg32k3a
rng_stream_runif <- function(size = 1, minimum = 0, maximum = 1, rng_stream = NULL) {
  if (methods::is(rng_stream, "RNGClass")) {
    rng_stream <- rng_stream$.getPointer()
  }
  if (!is.null(rng_stream)) {
    u <- rstream::r(rng_stream, size) * (maximum - minimum) + minimum
  } else {
    u <- stats::runif(n = size, min = minimum, max = maximum)
  }
  return(u)
}


#' Exponential random samples from `rstream` or `RGNClass` objects
#'
#' @description Sample from `rstream` or `RGNClass` objects
#' @param size Integer, number of samples
#' @param rate Positive number, the rate (i.e., 1/mean)
#' @param rng_stream (`rstream`) an `rstream` object or `NULL`
#'
#' @return a vector of exponential variates of size `size`
#' @export
#' @importClassesFrom rstream rstream.mrg32k3a
rng_stream_rexp <- function(size = 1, rate = 1, rng_stream = NULL) {
  if (methods::is(rng_stream, "RNGClass")) {
    rng_stream <- rng_stream$.getPointer()
  }
  if (!is.null(rng_stream)) {
    p <- rng_stream_runif(size = size, rng_stream = rng_stream)
    dt_ <- stats::qexp(p = p, rate = rate)
  } else {
    dt_ <- stats::rexp(n = size, rate = rate)
  }
  return(dt_)
}

#' Poisson random samples from `rstream` or `RGNClass` objects
#'
#' @description Sample from `rstream` or `RGNClass` objects
#' @param size Integer, number of samples
#' @param lambda Positive number, the mean
#' @param rng_stream (`rstream`) an `rstream` object or `NULL`
#'
#' @return a vector of counts of size `size`
#' @export
#' @importClassesFrom rstream rstream.mrg32k3a
rng_stream_rpois <- function(size = 1, lambda = 1, rng_stream = NULL) {
  if (methods::is(rng_stream, "RNGClass")) {
    rng_stream <- rng_stream$.getPointer()
  }
  if (!is.null(rng_stream)) {
    p <- rstream::r(rng_stream, 1)
    x <- stats::qpois(p = p, lambda = lambda)
  } else {
    x <- stats::rpois(n = size, lambda = lambda)
  }
  return(x)
}
