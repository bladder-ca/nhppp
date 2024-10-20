#' Simulate specific number of points from a homogeneous Poisson Point Process over (t_min, t_max]
#'
#' @description
#'   `r lifecycle::badge("deprecated")`
#' Use `ppp_exactly_n` instead.
#' @param size (int) the number of points to be simulated
#' @param range_t (vector, double) min and max of the time interval
#' @param rng_stream an `rstream` object
#'
#' @return a vector of event times of size `size`
#' @export
#' @keywords internal
#' @examples
#' x <- ppp_n(size = 10, range_t = c(0, 10))
ppp_n <- function(size, range_t = c(0, 10), rng_stream = NULL) {
  lifecycle::deprecate_warn(when = "0.5.0", what = "ppp_n()", with = "ppp_exactly_n()")
  U <- rng_stream_runif(size = size, minimum = range_t[1], maximum = range_t[2], rng_stream = rng_stream)
  if (size == 1) {
    return(U)
  }
  return(U[order(U, method = "shell")])
}
