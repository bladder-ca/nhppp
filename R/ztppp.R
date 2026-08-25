#' Simulate a zero-truncated homogeneous Poisson Point Process over (t_min, t_max]
#'
#' @description Simulate a constant-rate Poisson Point Process conditional on
#' observing at least `atleastK` events (`atleastK = 1`, the default, is the
#' zero-truncated process).
#'
#' @param t_min (scalar, double) lower bound of the time interval
#' @param t_max (scalar, double) upper bound of the time interval
#' @param rate (scalar, double) constant instantaneous rate
#' @param atmost1 boolean, report at most 1 event time (alias for `atmostK = 1`)
#' @param atmostK `NULL` or a positive integer: report only the earliest K
#'        event times of the conditioned process. Generalizes `atmost1`.
#' @param atleastK positive integer: condition on at least K events in
#'        `(t_min, t_max]`. `atleastK = 1` is the zero-truncated process.
#'
#' @return a vector of event times of size `size`
#' @export
#'
#' @examples
#' x <- ztppp(t_min = 0, t_max = 10, rate = 0.001)
ztppp <- function(rate, t_min, t_max, atmost1 = FALSE, atmostK = NULL, atleastK = 1) {
  atmostK <- .resolve_atmostK(atmost1, atmostK)
  atleastK <- .resolve_atleastK(atleast1 = FALSE, atleastK = atleastK)
  if (atleastK < 1L) stop("`atleastK` must be >= 1 for the truncated (zt) samplers")

  n <- rbtpois(n = 1, lambda = rate * (t_max - t_min), k = atleastK)
  tmp <- ppp_exactly_n(n = n, t_min = t_min, t_max = t_max)
  if (atmostK > 0L && atmostK < length(tmp)) {
    tmp <- tmp[seq_len(atmostK)]
  }
  return(tmp)
}
