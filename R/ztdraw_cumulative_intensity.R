#' Simulate from a zero-truncated non homogeneous Poisson Point Process (zt-NHPPP) from
#'    (t_min, t_max) (order statistics method)
#'
#' @description  Sample zero-truncated NHPPP times using the order statistics method,
#' optionally using an `rstream` generator
#' @param Lambda (function, double vector) a continuous increasing R to R map
#'               which is the integrated rate of the NHPPP
#' @param Lambda_inv (function, double vector) the inverse of `Lambda()`
#' @param t_min (double) the lower bound of the time interval
#' @param t_max (double) the upper bound of the time interval
#' @param atmost1 (boolean) report at most 1 event time (alias for `atmostK = 1`)
#' @param atmostK `NULL` or a positive integer: report only the earliest K
#'        event times of the conditioned process. Generalizes `atmost1`.
#' @param atleastK positive integer: condition on at least K events in the
#'        interval. `atleastK = 1` (default) is the zero-truncated process.
#'
#' @return a vector of at least `atleastK` event times
#' @export
ztdraw_cumulative_intensity <- function(Lambda,
                                        Lambda_inv,
                                        t_min,
                                        t_max,
                                        atmost1 = FALSE,
                                        atmostK = NULL,
                                        atleastK = 1) {
  atmostK <- .resolve_atmostK(atmost1, atmostK)
  atleastK <- .resolve_atleastK(atleast1 = FALSE, atleastK = atleastK)
  if (atleastK < 1L) stop("`atleastK` must be >= 1 for the truncated (zt) samplers")
  tmp_u <- ztppp(
    rate = 1, t_min = Lambda(t_min), t_max = Lambda(t_max),
    atmostK = if (atmostK > 0L) atmostK else NULL, atleastK = atleastK
  )
  return(Lambda_inv(tmp_u))
}
