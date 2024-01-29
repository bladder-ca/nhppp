#' Simulate a piecewise constant-rate Poisson Point Process over `(t_min, t_max]` (inversion method)
#' The intervals need not have the same length.
#'
#' @param times_vector (vector, double) `K+1` time points defining `K` intervals
#'        of constant rates:
#'             `[t_1 = range_t[1],       t_2)`: the first interval
#'             `[t_k,                t_{k+1})`: the `k`-th interval
#'             `[t_{K}, t_{K+1} = range_t[2])`: the `K`-th (last) interval
#' @param rates_vector (scalar, double) `K` constant rates, one per interval
#' @param rng_stream an `rstream` or `RNGClass` object
#' @param only1 boolean, draw at most 1 event time
#' @param zero_truncated boolean, draw at least 1 event time
#'
#' @return a vector of event times t
#'         if no events realize, it will have 0 length
#' @export
#'
#' @examples
#' x <- ppp_t_piecewise(rates_vector = rep(1, 5), times_vector = c(0:5))
#' @export
ppp_t_piecewise <- function(rates_vector = rep(1, 5),
                            times_vector = c(0:5),
                            rng_stream = NULL,
                            only1 = FALSE,
                            zero_truncated = FALSE) {
  len_times_vector <- length(times_vector)
  if (len_times_vector != (length(rates_vector) + 1)) stop("incompatible `times_vector` and `rates_vector` lengths")

  if (zero_truncated == FALSE) {
    ppp_t_fun <- ppp_t_orderstat
  } else {
    ppp_t_fun <- ztppp_t
  }

  if (len_times_vector == 2) {
    return(ppp_t_fun(range_t = times_vector, rate = rates_vector, rng_stream = rng_stream, only1 = only1))
  }

  time_H <- times_vector
  lambda <- c(0, rates_vector)
  time_L <- c(NA, time_H[1:(len_times_vector - 1)]) # data.table::shift(time_H)
  dtime <- time_H - time_L
  Lambda <- lambda * dtime
  Lambda[1] <- 0
  time_warped_H <- cumsum(Lambda)
  time_warped_L <- c(0, time_warped_H[1:(len_times_vector - 1)]) # data.table::shift(time_warped_H, n=1, type = "lag", fill = 0)

  times_warped <- ppp_t_fun(rate = 1, range_t = c(0, time_warped_H[len_times_vector]), rng_stream = rng_stream, only1 = only1)

  num_times <- length(times_warped)
  if (num_times == 0 || any(is.na(times_warped))) {
    return(numeric(0))
  }

  x_index <- rep(1:len_times_vector, num_times)
  t_index <- rep(1:num_times, each = len_times_vector)
  to_keep <- times_warped[t_index] >= time_warped_L[x_index] &
    times_warped[t_index] < time_warped_H[x_index]
  x_to_keep <- x_index[to_keep]
  t_to_keep <- t_index[to_keep]
  t_ <- ((times_warped[t_to_keep] - time_warped_L[x_to_keep]) /
    Lambda[x_to_keep]) * dtime[x_to_keep] + time_L[x_to_keep]

  return(t_[!is.na(t_)])
}
