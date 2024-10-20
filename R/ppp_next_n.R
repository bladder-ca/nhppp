#' Simulate n events from a homogeneous Poisson Point Process.
#'
#' @param rate scalar instantaneous rate
#' @param n scalar number of samples
#' @param t_min scalar for the starting time value
#' @param rng_stream  `r lifecycle::badge("deprecated")` an `rstream` object
#'
#' @return a vector with event times t (starting from t_min)
#' @export
#'
#' @examples
#' x <- ppp_next_n(n = 10, rate = 1, t_min = 0)
#' @importClassesFrom rstream rstream.mrg32k3a
ppp_next_n <- function(n = 1, rate = 1, t_min = 0, rng_stream = deprecated()) {
  if (lifecycle::is_present(rng_stream)) {
    lifecycle::deprecate_warn(when = "0.5.0", what = "ppp_next_n(rng_stream)")
    dt_ <- rng_stream_rexp(size = n, rate = rate, rng_stream = rng_stream)
  } else {
    dt_ <- stats::rexp(n = n, rate = rate)
  }

  t_ <- cumsum(dt_) + t_min
  return(t_)
}
