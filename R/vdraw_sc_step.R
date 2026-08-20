#' Validate and prepare the arguments of `vdraw_sc_step()` and `vztdraw_sc_step()`
#'
#' @description Shared validation for the vectorized step samplers with
#' arbitrary interval bounds. Returns the coerced rate matrix, the
#' `time_breaks` matrix (1 row if shared across point processes), and the
#' subinterval matrix (or `NULL` if no subinterval is requested).
#'
#' @param lambda_matrix (matrix) intensity rates, one per interval
#' @param Lambda_matrix (matrix) integrated intensity rates at the end of each interval
#' @param time_breaks (vector | matrix) interval bounds, see `vdraw_sc_step()`
#' @param t_min (scalar | vector | column matrix) optional subinterval lower bound
#' @param t_max (scalar | vector | column matrix) optional subinterval upper bound
#'
#' @return a list with elements `rate`, `is_cumulative`, `time_breaks`, `subinterval`
#' @noRd
.prep_vdraw_sc_step_args <- function(
    lambda_matrix = NULL,
    Lambda_matrix = NULL,
    time_breaks = NULL,
    t_min = NULL,
    t_max = NULL) {
  if (!is.null(lambda_matrix) && is.null(Lambda_matrix)) {
    rate <- lambda_matrix
    is_cumulative_rate <- FALSE
  } else if (is.null(lambda_matrix) && !is.null(Lambda_matrix)) {
    rate <- Lambda_matrix
    is_cumulative_rate <- TRUE
  } else {
    stop("lambda_matrix and Lambda_matrix cannot both be `NULL`")
  }
  mode(rate) <- "numeric"

  num_na <- sum(is.na(rate))
  if (num_na > 0) {
    rate_argument <- if (is_cumulative_rate) "Lambda_matrix" else "lambda_matrix"
    stop("The ", rate_argument, " contains ", num_na, " NA values")
  }

  if (is.null(time_breaks)) {
    stop("time_breaks cannot be `NULL`")
  }
  if (!is.matrix(time_breaks)) {
    time_breaks <- matrix(time_breaks, nrow = 1)
  }
  mode(time_breaks) <- "numeric"
  if (any(!is.finite(time_breaks))) {
    stop("time_breaks contains NA or non-finite values")
  }
  K1 <- ncol(time_breaks)
  if (K1 != ncol(rate) + 1) {
    stop(
      "time_breaks must have one more column than the [Lambda|lambda]_matrix ",
      "(K+1 interval bounds for K intervals)"
    )
  }
  if (!(nrow(time_breaks) %in% c(1L, nrow(rate)))) {
    stop("The (rows of) [Lambda|lambda]_matrix and (rows of) time_breaks imply different numbers of point processes to be sampled.")
  }
  if (any(time_breaks[, -1, drop = FALSE] <= time_breaks[, -K1, drop = FALSE])) {
    stop("time_breaks must be strictly increasing along each row")
  }

  if (is.null(t_min) && is.null(t_max)) {
    subinterval <- NULL
  } else {
    if (is.null(t_min)) t_min <- time_breaks[, 1, drop = FALSE]
    if (is.null(t_max)) t_max <- time_breaks[, K1, drop = FALSE]
    subinterval <- cbind(as.vector(t_min), as.vector(t_max))
    if (nrow(subinterval) > 1 && nrow(subinterval) != nrow(rate)) {
      stop("The (rows of) [Lambda|lambda]_matrix and (length of) [t_min|t_max] imply different numbers of point processes to be sampled.")
    }
    if (nrow(subinterval) == 1 && nrow(rate) != 1) {
      subinterval <- subinterval[rep(1, nrow(rate)), ]
    }
    stopifnot(
      all(subinterval[, 1] >= time_breaks[, 1]),
      all(subinterval[, 2] <= time_breaks[, K1]),
      all(subinterval[, 1] <= subinterval[, 2])
    )
  }

  return(list(
    rate = rate,
    is_cumulative = is_cumulative_rate,
    time_breaks = time_breaks,
    subinterval = subinterval
  ))
}


#' Vectorized sampling from NHPPPs with piecewise constant intensities
#' over arbitrary interval bounds (C++)
#'
#' @description
#' Simulate piecewise constant-rate Poisson Point Processes (inversion method)
#' where the intervals need not have the same length. The interval bounds are
#' given in `time_breaks`, either once for all point processes or per point
#' process. This generalizes `vdraw_sc_step_regular()`, which assumes
#' equal-length ("regular") intervals.
#'
#' @param lambda_matrix (matrix) intensity rates, one per interval
#' @param Lambda_matrix (matrix) integrated intensity rates at the end of each interval
#' @param time_breaks (vector | matrix) the bounds of the intervals over which
#'        the rates apply. With `K` intervals (`K = ncol([Lambda|lambda]_matrix)`),
#'        either a vector of `K+1` increasing values (the same bounds for all
#'        point processes) or a matrix with `K+1` columns whose i-th row holds
#'        the bounds for the i-th point process (a 1-row matrix is recycled for
#'        all point processes).
#' @param t_min (scalar | vector | column matrix) is the lower bound
#'        of a subinterval of `(time_breaks[, 1], time_breaks[, K+1]]`. If set,
#'        times are sampled from the subinterval.
#'        If omitted, it is equivalent to `time_breaks[, 1]`.
#' @param t_max (scalar | vector | column matrix) the upper bound
#'        of a subinterval of `(time_breaks[, 1], time_breaks[, K+1]]`. If set,
#'        times are sampled from the subinterval.
#'        If omitted, it is equivalent to `time_breaks[, K+1]`.
#' @param tol (scalar, double) tolerance for the number of events
#' @param atmost1 boolean, draw at most 1 event time
#' @param atmostB If not NULL, draw at most B (B>0) event times. NULL means ignore.
#' @param atleast1 boolean, draw at least 1 event time
#'
#' @return a matrix of event times t, with rows corresponding to the sampled point processes.
#'
#' @examples
#' # one set of interval bounds for all point processes
#' x <- vdraw_sc_step(
#'   lambda_matrix = matrix(rep(1, 50), nrow = 10),
#'   time_breaks = c(100, 100.5, 102, 106, 109, 110),
#'   atmost1 = FALSE
#' )
#'
#' # separate interval bounds per point process
#' x <- vdraw_sc_step(
#'   lambda_matrix = matrix(rep(1, 50), nrow = 10),
#'   time_breaks = matrix(rep(c(100, 100.5, 102, 106, 109, 110), each = 10), nrow = 10) + 0:9,
#'   atmost1 = FALSE
#' )
#' @export
vdraw_sc_step <- function(
    lambda_matrix = NULL,
    Lambda_matrix = NULL,
    time_breaks = NULL,
    t_min = NULL,
    t_max = NULL,
    tol = 10^-6,
    atmost1 = FALSE,
    atmostB = NULL,
    atleast1 = FALSE) {
  if (atleast1) {
    return(
      vztdraw_sc_step(
        lambda_matrix = lambda_matrix,
        Lambda_matrix = Lambda_matrix,
        time_breaks = time_breaks,
        t_min = t_min,
        t_max = t_max,
        atmost1 = atmost1
      )
    )
  }

  if (is.null(atmostB)) {
    atmostB <- 0 # has to be <=0 in the C++ argument to be ignored
  }

  args <- .prep_vdraw_sc_step_args(
    lambda_matrix = lambda_matrix,
    Lambda_matrix = Lambda_matrix,
    time_breaks = time_breaks,
    t_min = t_min,
    t_max = t_max
  )

  if (is.null(args$subinterval)) {
    Z <- .Call(
      `_nhppp_vdraw_sc_step_general`,
      args$rate, args$is_cumulative, args$time_breaks, tol, atmost1
    )
    # the whole-range kernel has no atmostB argument; event times are sorted
    # within a row, so keeping the first B columns keeps the earliest B events
    if (atmostB > 0 && ncol(Z) > atmostB) {
      Z <- Z[, 1:atmostB, drop = FALSE]
    }
    return(Z)
  }
  return(
    .Call(
      `_nhppp_vdraw_sc_step_general2`,
      args$rate, args$is_cumulative, args$time_breaks, args$subinterval,
      tol, atmost1, atmostB
    )
  )
}
