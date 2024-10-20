#' Simulate from a non homogeneous Poisson Point Process (NHPPP) from
#'    (t0, t_max) (thinning method) with piecewise constant_majorizer
#'
#' @description Sample NHPPP times using the thinning method
#' @param lambda (function) the instantaneous rate of the NHPPP.
#' A continuous function of time.
#' @param majorizer_vector (scalar, double) `K` constant majorizing rates, one per interval
#' @param time_breaks (vector, double) `K+1` time points defining `K` intervals
#'        of constant rates:
#'             `[t_1 = range_t[1],       t_2)`: the first interval
#'             `[t_k,                t_{k+1})`: the `k`-th interval
#'             `[t_{K}, t_{K+1} = range_t[2])`: the `K`-th (last) interval
#' @param atmost1 boolean, draw at most 1 event time
#'
#' @return a vector of event times (t_); if no events realize,
#'         a vector of length 0
#' @keywords internal
draw_intensity_step <- function(lambda,
                                majorizer_vector,
                                time_breaks,
                                atmost1 = FALSE) {
  len_lambda <- length(majorizer_vector)
  candidate_times <- draw_sc_step(lambda_vector = majorizer_vector, time_breaks = time_breaks, atmost1 = FALSE)
  num_candidates <- length(candidate_times)
  if (num_candidates == 0) {
    return(candidate_times)
  }
  u <- stats::runif(n = num_candidates, min = 0, max = 1)
  acceptance_prob <- lambda(candidate_times) /
    stats::approx(
      x = time_breaks[1:len_lambda],
      y = majorizer_vector,
      xout = candidate_times, method = "constant", rule = 2, f = 0
    )$y
  if (!all(acceptance_prob <= 1 + 10^-6)) stop("lambda > lambda_maj\n")
  if (atmost1) {
    t <- candidate_times[u < acceptance_prob][1]
    if (is.na(t)) {
      t <- numeric(0)
    }
  } else {
    t <- candidate_times[u < acceptance_prob]
  }
  return(t)
}
