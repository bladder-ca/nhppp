#' Vectorized simulation from a non homogeneous Poisson Point Process (NHPPP) from
#'    (t_min, t_max) (inversion method)
#'
#' @description  Sample NHPPP times using the inversion method,
#' optionally using an `rstream` generator object
#' @param Lambda (function, double vector) an increasing function
#'               which is the integrated rate of the NHPPP. It shoudl take a vectorized argument t for times and an optional arguments list
#' @param Lambda_args (list) optional arguments to pass to Lambda
#' @param Lambda_inv (function, double vector) the inverse of `Lambda()`, also in vectorized form
#' @param Lambda_inv_args (list) optional arguments to pass to Lambda_inv()
#' @param range_t (vector/matrix double) min and max of the time interval. If a vecor of 2 elements, we assume that 1 series of points is drawn.
#'        If a matrix, its rows are the number of point processes that should be drawn.
#' @param tol the tolerange for the calulations
#' @param atmost1 boolean, draw at most 1 event time
#'
#' @return a vector of event times (t_); if no events realize,
#'         a vector of length 0
#' @export
#'
vdraw_cumulative_intensity <- function(Lambda,
                                       Lambda_inv,
                                       Lambda_args = NULL,
                                       Lambda_inv_args = NULL,
                                       range_t = NULL,
                                       tol = 10^-6,
                                       atmost1 = FALSE) {
  # browser()
  if (!is.matrix(range_t)) {
    range_t <- matrix(range_t, ncol = 2)
  }
  N_rows <- nrow(range_t)
  range_L <- Lambda(range_t, Lambda_args = Lambda_args)

  if (isTRUE(atmost1)) {
    N_cols <- 1
  } else {
    N_cols <- max(stats::qpois(p = 1 - tol, lambda = 1 * (range_L[, 2] - range_L[, 1])))
  }

  warped_t <- matrix(stats::rexp(n = N_cols * N_rows, rate = 1), ncol = N_cols)
  matrix_cumsum_columns_inplace(warped_t)
  warped_t <- warped_t + range_L[, 1]
  for (col in 1:N_cols) {
    in_range_L <- (warped_t[, col] <= range_L[, 2])
    if (col > 1 && all(!in_range_L)) {
      warped_t <- warped_t[, 1:(col - 1)]
      break
    }
    warped_t[!in_range_L, col] <- NA
  }

  return(Lambda_inv(warped_t, Lambda_inv_args = Lambda_inv_args))
}
