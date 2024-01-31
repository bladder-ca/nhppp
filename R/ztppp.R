#' Simulate a zero-truncated homogeneous Poisson Point Process over (t_min, t_max]
#'
#' @param range_t (vector, double) min and max of the time interval
#' @param rate (scalar, double) constant instantaneous rate
#' @param rng_stream an `rstream` object
#' @param atmost1 boolean, draw at most 1 event time
#'
#' @return a vector of event times of size `size`
#' @export
#'
#' @examples
#' x <- ztppp(range_t = c(0, 10), rate = 0.001)
ztppp <- function(range_t = c(0, 10), rate = 1, rng_stream = NULL, atmost1 = FALSE) {
  n <- rng_stream_rztpois(size = 1, lambda = rate * (range_t[2] - range_t[1]), rng_stream = rng_stream)
  tmp <- ppp_n(size = n, range_t = range_t, rng_stream = rng_stream)
  if (atmost1 == TRUE) {
    return(tmp[1])
  }
  return(tmp)
}
