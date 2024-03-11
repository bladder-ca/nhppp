#' Sampling from NHPPPs with piecewise constant intensities
#' with same interval lengths (non-vectorized)
#'
#' @param Lambda_vector (scalar, double) `K` integrated intensity rates at the end of each interval
#' @param lambda_vector (scalar, double) `K` constant intensity rates, one per interval
#' @param range_t (vector, double) `t_min` and `t_max`
#' @param rng_stream an `rstream` object
#' @param atmost1 boolean, draw at most 1 event time
#' @param atleast1 boolean, draw at least 1 event time
#'
#' @return a vector of event times t
#'         if no events realize, it will have 0 length
#' @export
#'
#' @examples
#' x <- draw_sc_step_regular(Lambda_vector = 1:5, range_t = c(0, 5))
#' @export
draw_sc_step_regular <- function(Lambda_vector = NULL,
                                 lambda_vector = NULL,
                                 range_t = c(0, 10),
                                 rng_stream = NULL,
                                 atmost1 = FALSE,
                                 atleast1 = FALSE) {
  # browser()
  if (is.null(Lambda_vector) && !is.null(lambda_vector)) {
    Lambda_vector <- cumsum(lambda_vector)
  }
  n_intervals <- length(Lambda_vector)
  interval_length <- (range_t[2] - range_t[1]) / n_intervals
  Lambda_vector <- c(0, Lambda_vector)

  if (atleast1 == FALSE) {
    ppp_t_fun <- ppp_orderstat
  } else {
    ppp_t_fun <- ztppp
  }

  if (n_intervals == 1) {
    tau <- ppp_t_fun(range_t = range_t, rate = (Lambda_vector[2] - Lambda_vector[1]) / interval_length, rng_stream = rng_stream, atmost1 = atmost1)
    return(tau[!is.na(tau)])
  }

  tau <- ppp_t_fun(rate = 1, range_t = c(0, Lambda_vector[n_intervals + 1]), rng_stream = rng_stream, atmost1 = atmost1)

  n_events <- length(tau)
  if (n_events == 0 || any(is.na(tau))) {
    return(numeric(0))
  }

  tau_indices <- rep(1:n_events, each = n_intervals)
  Lambda_indices_low <- rep(1:n_intervals, n_events)
  Lambda_indices_high <- rep(2:(n_intervals + 1), n_events)

  tau_in_interval <- (tau[tau_indices] >= Lambda_vector[Lambda_indices_low] &
    tau[tau_indices] < Lambda_vector[Lambda_indices_high])



  t_ <- range_t[1] +
    interval_length * (
      (Lambda_indices_low[tau_in_interval] - 1) +
        (tau - Lambda_vector[Lambda_indices_low[tau_in_interval]]) /
          (
            Lambda_vector[Lambda_indices_high[tau_in_interval]] -
              Lambda_vector[Lambda_indices_low[tau_in_interval]]
          )
    )

  return(t_)
}
