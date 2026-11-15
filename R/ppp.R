#' Simulate a homogeneous Poisson Point Process in (t_min, t_max]
#'
#' @param rate (scalar, double) constant instantaneous rate
#' @param t_min (scalar, double) the lower bound of the time interval
#' @param t_max (scalar, double) the upper bound of the time interval
#' @param atmost1 boolean, report at most 1 event time (alias for
#'        `report_first_K = 1`)
#' @inheritParams draw_sc_step
#' @param tol the probability that we will have more than
#'        the drawn events in (t_min, t_max]
#'
#' @return a vector of event times t
#'         if no events realize, it will have 0 length
#' @export
#'
#' @examples
#' x <- ppp(rate = 1, t_min = 0, t_max = 10, tol = 10^-6)
ppp <- function(rate, t_min, t_max, atmost1 = FALSE, tol = 10^-6,
                report_first_K = NULL,
                report_last_K = NULL,
                atleast1 = FALSE,
                generate_at_least_K = NULL,
                generate_at_most_K = NULL) {
  rep_ <- .resolve_reporting(atmost1, report_first_K, report_last_K)
  gen_ <- .resolve_generation(atleast1, generate_at_least_K, generate_at_most_K)
  plain_fun <- function(atmost1) {
    if (isTRUE(atmost1)) {
      n <- 1
    } else {
      n <- stats::qpois(p = 1 - tol, lambda = rate * (t_max - t_min))
    }
    dt_ <- stats::rexp(n = n, rate = rate)
    t_ <- cumsum(dt_) + t_min
    return(t_[t_ <= t_max])
  }
  return(.run_scalar_sampler(
    rep_, gen_,
    plain_fun = plain_fun,
    zt_fun = function(...) ztppp(rate = rate, t_min = t_min, t_max = t_max, ...)
  ))
}
