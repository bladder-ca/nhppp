#' Sampling from NHPPPs with piecewise constant intensities
#' with same interval lengths (non-vectorized)
#'
#' @param Lambda_vector (scalar, double) `K` integrated intensity rates at the end of each interval
#' @param lambda_vector (scalar, double) `K` constant intensity rates, one per interval
#' @param t_min (scalar, double) lower bound of the time interval
#' @param t_max (scalar, double) upper bound of the time interval
#' @param atmost1 boolean, report at most 1 event time (alias for `atmostK = 1`)
#' @param atmostK `NULL` or a positive integer: report only the earliest K
#'        event times. Generalizes `atmost1`.
#' @param atleast1 boolean, condition on at least 1 event (alias for `atleastK = 1`)
#' @param atleastK `NULL` or a positive integer: condition on at least K events
#'        in the interval. Generalizes `atleast1`.
#'
#' @return a vector of event times t
#'         if no events realize, it will have 0 length
#' @export
#'
#' @examples
#' x <- draw_sc_step_regular(Lambda_vector = 1:5, t_min = 0, t_max = 5)
#' @export
draw_sc_step_regular <- function(Lambda_vector = NULL,
                                 lambda_vector = NULL,
                                 t_min = NULL,
                                 t_max = NULL,
                                 atmost1 = FALSE,
                                 atmostK = NULL,
                                 atleast1 = FALSE,
                                 atleastK = NULL) {
  stopifnot(!is.null(t_min) && !is.null(t_max))
  if (is.null(Lambda_vector) && !is.null(lambda_vector)) {
    Lambda_vector <- cumsum(lambda_vector)
  }
  n_intervals <- length(Lambda_vector)
  interval_length <- (t_max - t_min) / n_intervals
  Lambda_vector <- c(0, Lambda_vector)
  atmostK <- .resolve_atmostK(atmost1, atmostK)
  atleastK <- .resolve_atleastK(atleast1, atleastK)

  if (atleastK >= 1L) {
    ppp_t_fun <- function(rate, t_min, t_max, atmost1) {
      ztppp(
        rate = rate, t_min = t_min, t_max = t_max,
        atmostK = if (atmostK > 0L) atmostK else NULL, atleastK = atleastK
      )
    }
  } else {
    ppp_t_fun <- function(rate, t_min, t_max, atmost1) {
      z <- ppp2(rate = rate, t_min = t_min, t_max = t_max, atmost1 = atmost1)
      if (atmostK > 1L && length(z) > atmostK) z <- z[seq_len(atmostK)]
      return(z)
    }
  }
  atmost1 <- (atmostK == 1L)

  if (n_intervals == 1) {
    tau <- ppp_t_fun(rate = (Lambda_vector[2] - Lambda_vector[1]) / interval_length, t_min = t_min, t_max = t_max, atmost1 = atmost1)
    return(tau[!is.na(tau)])
  }

  tau <- ppp_t_fun(rate = 1, t_min = 0, t_max = Lambda_vector[n_intervals + 1], atmost1 = atmost1)

  n_events <- length(tau)
  if (n_events == 0 || any(is.na(tau))) {
    return(numeric(0))
  }

  tau_indices <- rep(1:n_events, each = n_intervals)
  Lambda_indices_low <- rep(1:n_intervals, n_events)
  Lambda_indices_high <- rep(2:(n_intervals + 1), n_events)

  tau_in_interval <- (tau[tau_indices] >= Lambda_vector[Lambda_indices_low] &
    tau[tau_indices] < Lambda_vector[Lambda_indices_high])



  t_ <- t_min +
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
