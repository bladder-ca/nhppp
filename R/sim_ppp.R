#' Simulate n events from a homogeneous Poisson Point Process.
#'
#' @param rate scalar instantaneous rate
#' @param n scalar number of samples
#' @param t_min scalar for the starting time value
#' @param rng_stream an `rstream` or `RNGClass` object
#'
#' @return a vector with event times t (starting from t_min)
#' @export
#'
#' @examples
#' x <- ppp_next_n(n = 10, rate = 1, t_min = 0)
#' @importClassesFrom rstream rstream.mrg32k3a
ppp_next_n <- function(n = 1, rate = 1, t_min = 0, rng_stream = NULL) {
  dt_ <- rng_stream_rexp(size = n, rate = rate, rng_stream = rng_stream)
  t_ <- cumsum(dt_) + t_min
  return(t_)
}


#' Simulate a homogeneous Poisson Point Process over (t_min, t_max]
#'
#' @param range_t (vector, double) min and max of the time interval
#' @param rate (scalar, double) constant instantaneous rate
#' @param tol the probability that we will have more than
#'        the drawn events in (t_min, t_max]
#' @param rng_stream an `rstream` or `RNGClass` object
#' @param only1 boolean, draw at most 1 event time
#'
#' @return a vector of event times t
#'         if no events realize, it will have 0 length
#' @export
#'
#' @examples
#' x <- ppp_t(range_t = c(0, 10), rate = 1, tol = 10^-6)
ppp_t <- function(range_t = c(0, 10), rate = 1, tol = 10^-6, rng_stream = NULL, only1 = FALSE) {
  # we expect lambda = t_max*rate events
  # we will draw n so that the probability that t>t_max is 1-tol
  if (isTRUE(only1)) {
    n <- 1
  } else {
    n <- stats::qpois(p = 1 - tol, lambda = rate * (range_t[2] - range_t[1]))
  }
  dt_ <- rng_stream_rexp(size = n, rate = rate, rng_stream = rng_stream)
  t_ <- cumsum(dt_) + range_t[1]
  return(t_[t_ <= range_t[2]])
}



#' Simulate a zero-truncated homogeneous Poisson Point Process over (t_min, t_max]
#'
#' @param range_t (vector, double) min and max of the time interval
#' @param rate (scalar, double) constant instantaneous rate
#' @param rng_stream an `rstream` or `RNGClass` object
#' @param only1 boolean, draw at most 1 event time
#'
#' @return a vector of event times of size `size`
#' @export
#'
#' @examples
#' x <- ztppp_t(range_t = c(0, 10), rate = 0.001)
ztppp_t <- function(range_t = c(0, 10), rate = 1, rng_stream = NULL, only1 = FALSE) {
  n <- rng_stream_rztpois(size = 1, lambda = rate * (range_t[2] - range_t[1]), rng_stream = rng_stream)
  tmp <- ppp_n(size = n, range_t = range_t, rng_stream = rng_stream)
  if (only1 == TRUE) {
    return(tmp[1])
  }
  return(tmp)
}


#' Simulate a homogeneous Poisson Point Process over (t_min, t_max] (order statistics method)
#'
#' @param range_t (vector, double) min and max of the time interval
#' @param rate (scalar, double) constant instantaneous rate
#' @param rng_stream an `rstream` or `RNGClass` object
#' @param only1 boolean, draw at most 1 event time
#'
#' @return a vector of event times t
#'         if no events realize, it will have 0 length
#' @export
#'
#' @examples
#' x <- ppp_t_orderstat(range_t = c(0, 10), rate = 1)
ppp_t_orderstat <- function(range_t = c(0, 10), rate = 1, rng_stream = NULL, only1 = FALSE) {
  # we expect lambda = t_max*rate events
  n <- rng_stream_rpois(size = 1, lambda = rate * (range_t[2] - range_t[1]), rng_stream = rng_stream)
  tmp <- ppp_n(size = n, range_t = range_t, rng_stream = rng_stream)
  if (only1 == TRUE) {
    return(tmp[1])
  }
  return(tmp)
}



#' Simulate specific number of points from a homogeneous Poisson Point Process over (t_min, t_max]
#'
#' @param size (int) the number of points to be simulated
#' @param range_t (vector, double) min and max of the time interval
#' @param rng_stream an `rstream` or `RNGClass` object
#'
#' @return a vector of event times of size `size`
#' @export
#'
#' @examples
#' x <- ppp_n(size = 10, range_t = c(0, 10))
ppp_n <- function(size, range_t = c(0, 10), rng_stream = NULL) {
  U <- rng_stream_runif(size = size, minimum = range_t[1], maximum = range_t[2], rng_stream = rng_stream)
  if (size == 1) {
    return(U)
  }
  return(U[order(U, method = "shell")])
}

#' Simulate a piecewise constant-rate Poisson Point Process over (t_min, t_max] (inversion method)
#'
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
  stopifnot(len_times_vector == (length(rates_vector) + 1))

  if (zero_truncated == FALSE) {
    ppp_t_fun <- ppp_t_orderstat
  } else {
    ppp_t_fun <- ztppp_t
  }

  if (len_times_vector == 2) {
    return(ppp_t_fun(range_t = times_vector, rate = rates_vector, rng_stream = rng_stream, only1 = only1))
  }
  stopifnot(!is.unsorted(times_vector))

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
