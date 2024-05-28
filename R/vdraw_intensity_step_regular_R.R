#' Vectorized sampling from a non homogeneous Poisson Point Process (NHPPP) from
#'    an interval (thinning method) with piecewise constant_majorizers (R)
#'
#' Vectorized sampling from a non homogeneous Poisson Point Process (NHPPP) from
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
#' @param force_zt_majorizer boolean, force the use of the zero-truncated majorizer; - used for flexibility in calling from other functions. Keep the default `FALSE` unless you know what you are doing.
#' @param ... (any) other arguments (ignored  -- used for flexibility in calling from other functions)
#'
#' @return a matrix of event times (columns) per draw (rows)
#'         NAs are structural empty spots
#' @examples
#' Z <- vdraw_intensity_step_regular_R(
#'   lambda = function(x, lambda_args = NULL) 0.1 * x,
#'   range_t = c(1, 10),
#'   lambda_maj_matrix = matrix(rep(1, 5), nrow = 1)
#' )
#' @export
vdraw_intensity_step_regular_R <- function(lambda = NULL,
                                           lambda_args = NULL,
                                           Lambda_maj_matrix = NULL,
                                           lambda_maj_matrix = NULL,
                                           range_t = NULL,
                                           tol = 10^-6,
                                           atmost1 = FALSE,
                                           force_zt_majorizer = FALSE,
                                           ...) {
  # browser()
  if (!is.null(Lambda_maj_matrix)) {
    mode(Lambda_maj_matrix) <- "numeric"
    n_intervals <- ncol(Lambda_maj_matrix)
    n_draws <- nrow(Lambda_maj_matrix)
  } else if (!is.null(lambda_maj_matrix)) {
    mode(lambda_maj_matrix) <- "numeric"
    n_intervals <- ncol(lambda_maj_matrix)
    n_draws <- nrow(lambda_maj_matrix)
  }
  range_t <- make_range_t_matrix(range_t = range_t, n_rows = n_draws)
  interval_duration <- (range_t[, 2] - range_t[, 1]) / n_intervals


  if (is.null(Lambda_maj_matrix)) {
    Lambda_maj_matrix <- mat_cumsum_columns(lambda_maj_matrix)
  }

  # Towards the end, I need lambda_maj_matrix
  # make this a helper function
  if (is.null(lambda_maj_matrix)) {
    lambda_maj_matrix <- matrix(0, ncol = n_intervals, nrow = n_draws)
    lambda_maj_matrix[, 1] <- Lambda_maj_matrix[, 1] / interval_duration
    for (col in 2:n_intervals) {
      lambda_maj_matrix[, col] <- (Lambda_maj_matrix[, col] - Lambda_maj_matrix[, col - 1]) / interval_duration
    }
  }
  mode(lambda_maj_matrix) <- "numeric"

  vdraw_fun <- vdraw_sc_step_regular_cpp
  if (force_zt_majorizer) {
    vdraw_fun <- vztdraw_sc_step_regular_R
  }

  Z_star <- vdraw_fun(
    lambda_matrix = lambda_maj_matrix,
    range_t = range_t,
    tol = tol,
    atmost1 = FALSE
  )

  n_max_events <- ncol(Z_star)
  U <- matrix(stats::runif(length(Z_star)), ncol = n_max_events)

  # Extracts correct majorising values
  lambda_maj_idx <- ceiling((Z_star - range_t[, rep(1, n_max_events)]) / interval_duration)

  # browser()
  idx_adjusted <- as.vector(lambda_maj_idx + (1:nrow(lambda_maj_matrix) - 1) * ncol(lambda_maj_matrix))
  lambda_maj <- matrix(
    t(lambda_maj_matrix)[idx_adjusted],
    nrow = nrow(lambda_maj_idx), ncol = ncol(lambda_maj_idx), byrow = FALSE
  )

  accept <- ifelse(lambda(Z_star, lambda_args) / lambda_maj > U, TRUE, NA)
  Z <- Z_star * accept
  Z_sorted <- matrix(
    Z[order(row(Z), is.na(Z), method = "radix")], # shifts non-NAs to the left
    nrow = nrow(Z), ncol = ncol(Z), byrow = TRUE
  )

  if (ncol(Z_sorted) > 1) {
    Z_sorted <- Z_sorted[, colSums(!is.na(Z_sorted)) > 0, drop = FALSE] # removes empty columns after the shift
  }

  if (atmost1) {
    return(Z_sorted[, 1, drop = FALSE])
  } else {
    return(Z_sorted)
  }
}
