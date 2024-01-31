#' Simulate a piecewise constant-rate Poisson Point Process over `(t_min, t_max]` (inversion method)
#' The intervals need not have the same length.
#'
#' @param times_vector (vector, double) `K+1` time points defining `K` intervals
#'        of constant rates:
#'             `[t_1 = range_t[1],       t_2)`: the first interval
#'             `[t_k,                t_{k+1})`: the `k`-th interval
#'             `[t_{K}, t_{K+1} = range_t[2])`: the `K`-th (last) interval
#' @param lambda_vector (scalar, double) `K` constant rates, one per interval
#' @param rng_stream an `rstream` object
#' @param atmost1 boolean, draw at most 1 event time
#' @param atleast1 boolean, draw at least 1 event time
#'
#' @return a vector of event times t
#'         if no events realize, it will have 0 length
#' @export
#'
#' @examples
#' x <- draw_sc_step(lambda_vector = rep(1, 5), times_vector = c(0:5))
#' @export
draw_sc_step <- function(lambda_vector = rep(1, 5),
                         times_vector = c(0:5),
                         rng_stream = NULL,
                         atmost1 = FALSE,
                         atleast1 = FALSE) {
  len_times_vector <- length(times_vector)
  if (len_times_vector != (length(lambda_vector) + 1)) stop("incompatible `times_vector` and `lambda_vector` lengths")

  if (atleast1 == FALSE) {
    ppp_t_fun <- ppp_orderstat
  } else {
    ppp_t_fun <- ztppp
  }

  if (len_times_vector == 2) {
    return(ppp_t_fun(range_t = times_vector, rate = lambda_vector, rng_stream = rng_stream, atmost1 = atmost1))
  }

  time_H <- times_vector
  lambda <- c(0, lambda_vector)
  time_L <- c(NA, time_H[1:(len_times_vector - 1)]) # data.table::shift(time_H)
  dtime <- time_H - time_L
  Lambda <- lambda * dtime
  Lambda[1] <- 0
  time_warped_H <- cumsum(Lambda)
  time_warped_L <- c(0, time_warped_H[1:(len_times_vector - 1)]) # data.table::shift(time_warped_H, n=1, type = "lag", fill = 0)

  times_warped <- ppp_t_fun(rate = 1, range_t = c(0, time_warped_H[len_times_vector]), rng_stream = rng_stream, atmost1 = atmost1)

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
