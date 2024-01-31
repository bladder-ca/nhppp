#' Simulate from a zero-truncated non homogeneous Poisson Point Process (NHPPP) from
#'    (t0, t_max) (thinning method) with piecewise constant_majorizer
#'
#' @description Sample zero-truncated NHPPP times using the thinning method, optionally using
#' an `rstream` generator
#' @param lambda (function) the instantaneous rate of the NHPPP.
#' A continuous function of time.
#' @param lambda_maj_vector (scalar, double) `K` constant majorizing rates, one per interval
#' @param times_vector (vector, double) `K+1` time points defining `K` intervals
#'        of constant rates:
#'             `[t_1 = range_t[1],       t_2)`: the first interval
#'             `[t_k,                t_{k+1})`: the `k`-th interval
#'             `[t_{K}, t_{K+1} = range_t[2])`: the `K`-th (last) interval
#' @param rng_stream (`rstream`) an `rstream` object or `NULL`
#' @param atmost1 boolean, draw at most 1 event time
#'
#' @return a vector of event times (t_) with at least one element
#' @export
#'
#' @examples
#' x <- ztdraw_intensity_step(lambda = function(t) exp(.02 * t))
ztdraw_intensity_step <- function(lambda,
                                  lambda_maj_vector = lambda(1:10),
                                  times_vector = 0:10,
                                  rng_stream = NULL,
                                  atmost1 = FALSE) {
  len_lambda <- length(lambda_maj_vector)

  lambda_maj_fun <- stats::approxfun(
    x = times_vector[1:len_lambda],
    y = lambda_maj_vector, method = "constant", rule = 2, f = 0
  )

  while (TRUE) {
    candidate_times <- draw_sc_step(lambda_vector = lambda_maj_vector, times_vector = times_vector, rng_stream = rng_stream, atmost1 = FALSE, atleast1 = TRUE)
    u <- rng_stream_runif(size = length(candidate_times), minimum = 0, maximum = 1, rng_stream = rng_stream)
    acceptance_prob <- lambda(candidate_times) / lambda_maj_fun(candidate_times)
    if (!all(acceptance_prob <= 1 + 10^-6)) stop("lambda > lambda_maj\n")
    candidate_times <- candidate_times[u < acceptance_prob]
    if (length(candidate_times) > 0) {
      if (atmost1) {
        return(candidate_times[1])
      } else {
        return(candidate_times)
      }
    }
  }
}
