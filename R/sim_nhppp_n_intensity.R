#' Simulate `size` samples from a non homogeneous Poisson Point Process (NHPPP) from
#'    (t0, t_max) (thinning method)
#'
#' @description Sample NHPPP intensity times using the thinning method, optionally using
#' an `rstream` generator or a `Kystis` `RNGClass` object.
#'
#' @param size (double) the number of samples.
#' @param lambda (function) the instantaneous rate of the NHPPP.
#' A continuous function of time.
#' @param lambda_maj (double, vector) the intercept and optional slope of the majorizing
#' linear function in `range_t`.
#' @param range_t (vector, double) min and max of the time interval.
#' @param rng_stream (`rstream`) an `rstream` or `RNGClass` object, or `NULL`
#'
#' @return a vector of event times (t_); if no events realize,
#'         a vector of length 0
#' @export
#'
#' @examples
#' x <- nhppp_n_intensity(23, lambda = function(t) 1 + sin(t))
nhppp_n_intensity <- function(size,
                              lambda,
                              lambda_maj = NULL,
                              range_t = c(0, 10),
                              rng_stream = NULL) {
  if (is.null(lambda_maj)) {
    alpha <- stats::optimize(
      f = function(x) lambda(x),
      interval = range_t,
      maximum = TRUE
    )$objective * 1.2
    beta <- 0
  } else if (length(lambda_maj) == 1) {
    alpha <- lambda_maj[1]
    beta <- 0
  } else if (length(lambda_maj) == 2) {
    alpha <- lambda_maj[1]
    beta <- lambda_maj[2]
  }
  times <- c()
  additional_samples <- size
  counter = 0
  while (additional_samples > 0) {
    counter = counter + 1
    candidate_times <-
      nhppp_n_intensity_linear(size = additional_samples, alpha = alpha, beta = beta, range_t = range_t, rng_stream = rng_stream)

    u <- rng_stream_runif(size = additional_samples, minimum = 0, maximum = 1, rng_stream = rng_stream)
    acceptance_prob <- lambda(candidate_times) / (alpha + beta * candidate_times)
    stopifnot(all(acceptance_prob <= 1))
    accepted <- (u <= acceptance_prob)
    times <- c(times, candidate_times[accepted])
    additional_samples <- additional_samples - sum(accepted)
  }

  if(counter == 1 || size == 1) {return(times)}
  return(times[order(times, method = "shell")])
}
