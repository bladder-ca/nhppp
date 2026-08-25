#' Simulate a piecewise constant-rate Poisson Point Process over `(t_min, t_max]` (inversion method)
#' The intervals need not have the same length.
#'
#' @param lambda_vector (scalar, double) `K` constant rates, one per interval
#' @param time_breaks (vector, double) `K+1` time points defining `K` intervals
#'        of constant rates:
#'             `[t_1 = range_t[1],       t_2)`: the first interval
#'             `[t_k,                t_{k+1})`: the `k`-th interval
#'             `[t_{K}, t_{K+1} = range_t[2])`: the `K`-th (last) interval
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
#' x <- draw_sc_step(lambda_vector = rep(1, 5), time_breaks = c(0:5))
#' @export
draw_sc_step <- function(lambda_vector,
                         time_breaks,
                         # rng_stream = NULL,
                         atmost1 = FALSE,
                         atmostK = NULL,
                         atleast1 = FALSE,
                         atleastK = NULL) {
  len_time_breaks <- length(time_breaks)
  if (len_time_breaks != (length(lambda_vector) + 1)) stop("incompatible `time_breaks` and `lambda_vector` lengths")
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

  if (len_time_breaks == 2) {
    return(ppp_t_fun(t_min = time_breaks[1], t_max = time_breaks[2], rate = lambda_vector, atmost1 = atmost1))
  }

  time_H <- time_breaks
  lambda <- c(0, lambda_vector)
  time_L <- c(NA, time_H[1:(len_time_breaks - 1)]) # data.table::shift(time_H)
  dtime <- time_H - time_L
  Lambda <- lambda * dtime
  Lambda[1] <- 0
  time_warped_H <- cumsum(Lambda)
  time_warped_L <- c(0, time_warped_H[1:(len_time_breaks - 1)]) # data.table::shift(time_warped_H, n=1, type = "lag", fill = 0)

  times_warped <- ppp_t_fun(rate = 1, t_min = 0, t_max = time_warped_H[len_time_breaks], atmost1 = atmost1)

  num_times <- length(times_warped)
  if (num_times == 0 || any(is.na(times_warped))) {
    return(numeric(0))
  }

  x_index <- rep(1:len_time_breaks, num_times)
  t_index <- rep(1:num_times, each = len_time_breaks)
  to_keep <- times_warped[t_index] >= time_warped_L[x_index] &
    times_warped[t_index] < time_warped_H[x_index]
  x_to_keep <- x_index[to_keep]
  t_to_keep <- t_index[to_keep]
  t_ <- ((times_warped[t_to_keep] - time_warped_L[x_to_keep]) /
    Lambda[x_to_keep]) * dtime[x_to_keep] + time_L[x_to_keep]

  return(t_[!is.na(t_)])
}
