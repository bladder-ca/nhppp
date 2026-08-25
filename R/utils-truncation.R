#' Resolve the `atmost1`/`atmostK` pair into the C++ argument
#'
#' @description `atmostK` (report only the earliest K events) generalizes
#' `atmost1`; the boolean is kept as an alias for `atmostK = 1`. Returns an
#' integer with 0 meaning "no cap" (the C++ kernels treat `<= 0` as off).
#'
#' @param atmost1 boolean, report at most 1 event time
#' @param atmostK `NULL` or a positive integer scalar
#' @noRd
.resolve_atmostK <- function(atmost1 = FALSE, atmostK = NULL) {
  if (is.null(atmostK)) {
    return(if (isTRUE(atmost1)) 1L else 0L)
  }
  atmostK <- as.integer(atmostK)
  if (length(atmostK) != 1L || is.na(atmostK) || atmostK < 1L) {
    stop("`atmostK` must be a positive integer scalar")
  }
  if (isTRUE(atmost1) && atmostK != 1L) {
    stop("`atmost1 = TRUE` contradicts `atmostK = ", atmostK, "`; specify one of the two")
  }
  return(atmostK)
}

#' Resolve the `atleast1`/`atleastK` pair into the C++ argument
#'
#' @description `atleastK` (condition on at least K events) generalizes
#' `atleast1`; the boolean is kept as an alias for `atleastK = 1`. Returns an
#' integer with 0 meaning "no conditioning" (untruncated sampler).
#'
#' @param atleast1 boolean, condition on at least 1 event
#' @param atleastK `NULL` or a positive integer scalar
#' @noRd
.resolve_atleastK <- function(atleast1 = FALSE, atleastK = NULL) {
  if (is.null(atleastK)) {
    return(if (isTRUE(atleast1)) 1L else 0L)
  }
  atleastK <- as.integer(atleastK)
  if (length(atleastK) != 1L || is.na(atleastK) || atleastK < 1L) {
    stop("`atleastK` must be a positive integer scalar")
  }
  if (isTRUE(atleast1) && atleastK != 1L) {
    stop("`atleast1 = TRUE` contradicts `atleastK = ", atleastK, "`; specify one of the two")
  }
  return(atleastK)
}

#' Resolve `budget_cap`, honoring the deprecated `atmostB` alias
#'
#' @description `budget_cap` (formerly `atmostB`) caps the computational
#' event budget of the vectorized kernels. It is an approximation knob (it
#' truncates the extreme tail of the event-count distribution together with
#' the `1 - tol` quantile bound), not an exact contract like `atmostK`.
#' Returns an integer with 0 meaning "no cap".
#' @noRd
.resolve_budget_cap <- function(budget_cap = NULL, atmostB = NULL) {
  if (!is.null(atmostB)) {
    warning("`atmostB` is deprecated; use `budget_cap`", call. = FALSE)
    if (is.null(budget_cap)) {
      budget_cap <- atmostB
    } else if (budget_cap != atmostB) {
      stop("`atmostB` (deprecated) and `budget_cap` disagree; use only `budget_cap`")
    }
  }
  if (is.null(budget_cap)) {
    return(0L)
  }
  budget_cap <- as.integer(budget_cap)
  if (length(budget_cap) != 1L || is.na(budget_cap) || budget_cap < 1L) {
    stop("`budget_cap` must be a positive integer scalar")
  }
  return(budget_cap)
}

# Note: `atleastK > atmostK` is deliberately allowed — conditioning on at
# least K events and reporting only the earliest M < K is well-defined (the
# zt kernels draw the conditioned count N, then report the M smallest order
# statistics).
