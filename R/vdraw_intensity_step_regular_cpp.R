#' Vectorized sampling from a non homogeneous Poisson Point Process (NHPPP) given
#'    the intensity function with piecewise constant_majorizers (C++)
#'
#' @description
#' Vectorized sampling from a non homogeneous Poisson Point Process (NHPPP) using the intensity
#' function and piecewise constant_majorizers.
#'    The majorizers are step functions over equal-length time intevals.
#'
#' @param lambda (function) intensity function, vectorized
#' @param lambda_args (list) optional arguments to pass to `lambda`
#' @param Lambda_maj_matrix (matrix) integrated intensity rates at the end of each interval
#' @param lambda_maj_matrix (matrix) intensity rates, one per interval
#' @param rate_matrix_t_min (scalar | vector | column matrix) is the lower bound
#'        of the time interval for each row of (Lambda|lambda)_maj_matrix.
#'        The length of this argument is the number of point processes that should be drawn.
#' @param rate_matrix_t_max (scalar | vector | column matrix) the upper bound
#'        of the time interval for each row of (Lambda|lambda)_maj_matrix.
#'        The length of this argument is the number of point processes that should be drawn.
#' @param t_min (scalar | vector | column matrix) is the lower bound
#'        of a subinterval of (rate_matrix_t_min, rate_matrix_t_max]. If set,
#'        times are sampled from the subinterval.
#'        If omitted, it is equivalent to `rate_matrix_t_min`.
#' @param t_max (scalar | vector | column matrix) is the upper bound
#'        of a subinterval of (rate_matrix_t_min, rate_matrix_t_max]. If set,
#'        times are sampled from the subinterval.
#'        If omitted, it is equivalent to `rate_matrix_t_max`.
#' @param tol (scalar, double) tolerance for the number of events
#' @param atmost1 boolean, draw at most 1 event time
#' @param atmostB If not NULL, draw at most B (B>0) event times. NULL means ignore.
#'
#' @return a matrix of event times (columns) per draw (rows)
#'         NAs are structural empty spots
#' @keywords internal
vdraw_intensity_step_regular_cpp <- function(lambda = NULL,
                                             lambda_args = NULL,
                                             Lambda_maj_matrix = NULL,
                                             lambda_maj_matrix = NULL,
                                             rate_matrix_t_min = NULL,
                                             rate_matrix_t_max = NULL,
                                             t_min = NULL,
                                             t_max = NULL,
                                             tol = 10^-6,
                                             atmost1 = FALSE,
                                             atmostB = NULL) {
  if (is.null(atmostB)) {
    atmostB <- 0 # has to be <=0 to be ignored
  }

  if (!is.null(lambda_maj_matrix) && is.null(Lambda_maj_matrix)) {
    rate <- lambda_maj_matrix
    is_cumulative_rate <- FALSE
  } else if (is.null(lambda_maj_matrix) && !is.null(Lambda_maj_matrix)) {
    rate <- Lambda_maj_matrix
    is_cumulative_rate <- TRUE
  } else {
    stop("lambda_maj_matrix and Lambda_maj_matrix cannot both be `NULL`")
  }
  mode(rate) <- "numeric"

  range_t <- cbind(as.vector(rate_matrix_t_min), as.vector(rate_matrix_t_max))
  if (nrow(range_t) > 1 && nrow(range_t) != nrow(rate)) {
    stop("The (rows of) [Lambda|lambda]_maj_matrix and (length of) [rate_matrix_t_min|rate_matrix_t_max] imply different numbers of point processes to be sampled.")
  }
  if (nrow(range_t) == 1 && nrow(rate) != 1) {
    range_t <- range_t[rep(1, nrow(rate)), ]
  }


  if (is.null(lambda_args)) {
    l_ <- lambda
  } else {
    l_ <- function(X, ...) {
      return(lambda(X, lambda_args))
    }
  }


  if (is.null(t_min) && is.null(t_max)) {
    use_subinterval <- FALSE
    subinterval <- range_t
  } else {
    use_subinterval <- TRUE
    if (is.null(t_min)) t_min <- range_t[, 1, drop = FALSE]
    if (is.null(t_max)) t_max <- range_t[, 2, drop = FALSE]
    subinterval <- cbind(as.vector(t_min), as.vector(t_max))
    if (nrow(subinterval) > 1 && nrow(subinterval) != nrow(rate)) {
      stop("The (rows of) [Lambda|lambda]_maj_matrix and (length of) [t_min|t_max] imply different numbers of point processes to be sampled.")
    }
    if (nrow(subinterval) == 1 && nrow(rate) != 1) {
      subinterval <- subinterval[rep(1, nrow(rate)), ]
    }
    stopifnot(all(subinterval[, 1] >= range_t[, 1]), all(subinterval[, 2] <= range_t[, 2]))
  }


  return(
    .Call(
      `_nhppp_vdraw_intensity_step_regular`, l_,
      rate, is_cumulative_rate, range_t, subinterval, use_subinterval, tol, atmost1, atmostB
    )
  )
}
