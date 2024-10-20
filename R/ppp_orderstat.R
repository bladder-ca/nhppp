#' Simulate a homogeneous Poisson Point Process over (t_min, t_max] (order statistics method)
#'
#' @description
#'   `r lifecycle::badge("deprecated")`
#' Use `ppp2` instead.
#' @param range_t (vector, double) min and max of the time interval
#' @param rate (scalar, double) constant instantaneous rate
#' @param rng_stream an `rstream` object
#' @param atmost1 boolean, draw at most 1 event time
#'
#' @return a vector of event times t
#'         if no events realize, it will have 0 length
#' @export
#'
#' @keywords internal
#' @examples
#' x <- ppp_orderstat(range_t = c(0, 10), rate = 1)
ppp_orderstat <- function(range_t = c(0, 10), rate = 1, rng_stream = NULL, atmost1 = FALSE) {
  # we expect lambda = t_max*rate events
  lifecycle::deprecate_warn(when = "0.5.0", what = "ppp_orderstat()", with = "ppp2()")
  n <- rng_stream_rpois(size = 1, lambda = rate * (range_t[2] - range_t[1]), rng_stream = rng_stream)
  tmp <- ppp_n(size = n, range_t = range_t, rng_stream = rng_stream)
  if (atmost1 == TRUE) {
    return(tmp[1])
  }
  return(tmp)
}
