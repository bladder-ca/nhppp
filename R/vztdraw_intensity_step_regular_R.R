#' Vectorized sampling from a zero-truncated non homogeneous Poisson Point Process (NHPPP) from
#'    an interval (thinning method) with piecewise constant_majorizers (R)
#'
#' Vectorized sampling from a zero-truncated non homogeneous Poisson Point Process (NHPPP) from
#'    an interval (thinning method) with piecewise constant_majorizers.
#'    The majorizers are step functions over equal-length time intevals.
#'
#' @param lambda (function) a vectorized intensity function, with one or two arguments.
#'  The first is time. The optional second is a named list with additional arguments.
#' @param lambda_args (list) optional list of named arguments for `lambda()`
#' @param Lambda_maj_matrix (matrix) for the majorizeintegrated intensity rates at the end of each interval
#' @param lambda_maj_matrix (matrix) intensity rates, one per interval
#' @param range_t (vector, or matrix) `t_min` and `t_max`, possibly vectorized
#' @param tol (scalar, double) tolerance for the number of events
#' @param atmost1 boolean, draw at most 1 event time
#' @param ... (any) other arguments (ignored  -- used for flexibility in calling from other functions)
#'
#' @return a matrix of event times (columns) per draw (rows)
#'         NAs are structural empty spots
#' @export
#'
#' @examples
#' Z <- vztdraw_intensity_step_regular_R(
#'   lambda = function(x, lambda_args = NULL) 0.1 * x,
#'   range_t = c(1, 10),
#'   lambda_maj_matrix = matrix(rep(1, 5), nrow = 1)
#' )
#' @export
vztdraw_intensity_step_regular_R <- function(lambda = NULL,
                                             lambda_args = NULL,
                                             Lambda_maj_matrix = NULL,
                                             lambda_maj_matrix = NULL,
                                             range_t = NULL,
                                             tol = 10^-6,
                                             atmost1 = FALSE,
                                             ...) {
  # browser()
  Z <- vdraw_intensity_step_regular_R(
    lambda = lambda,
    lambda_args = lambda_args,
    Lambda_maj_matrix = Lambda_maj_matrix,
    lambda_maj_matrix = lambda_maj_matrix,
    range_t = range_t,
    tol = tol,
    atmost1 = atmost1,
    force_zt_majorizer = TRUE
  ) # force_zt_majorizer)

  range_t <- make_range_t_matrix(range_t = range_t, n_rows = nrow(Z))


  has_no_times <- is.na(Z[, 1])
  max_events <- ncol(Z)

  while (sum(has_no_times) > 0) {
    Z_add <- vdraw_intensity_step_regular_R(
      lambda = lambda,
      lambda_args = lambda_args,
      Lambda_maj_matrix = Lambda_maj_matrix[has_no_times, , drop = FALSE],
      lambda_maj_matrix = lambda_maj_matrix[has_no_times, , drop = FALSE],
      range_t = range_t[has_no_times, , drop = FALSE],
      tol = tol,
      atmost1 = atmost1,
      force_zt_majorizer = TRUE
    ) # force_zt_majorizer)

    diff_cols <- ncol(Z_add) - ncol(Z)
    if (diff_cols > 0) {
      Z <- matrix(
        c(as.vector(Z), rep(NA, times = abs(diff_cols) * nrow(Z))),
        ncol = ncol(Z_add), nrow = nrow(Z), byrow = FALSE
      )
    }
    if (diff_cols < 0) {
      Z_add <- matrix(
        c(as.vector(Z_add), rep(NA, times = abs(diff_cols) * nrow(Z_add))),
        ncol = ncol(Z), nrow = nrow(Z_add), byrow = FALSE
      )
    }

    Z[has_no_times, ] <- Z_add
    has_no_times <- is.na(Z[, 1])
  }
  return(Z)
}
