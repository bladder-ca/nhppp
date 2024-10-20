#' Vectorized simulation from a non homogeneous Poisson Point Process (NHPPP) from
#'    (t_min, t_max) given the cumulative intensity function and its inverse
#'
#' @description  Sample NHPPP times using the cumulative intensity function and its inverse.
#' @param Lambda (function, double vector) an increasing function
#'        which is the integrated rate of the NHPPP.
#'        It should take a vectorized argument t for times and an optional arguments list.
#' @param Lambda_inv (function, double vector) the inverse of `Lambda()`, also in vectorized form
#'        It should take a vectorized argument z and an optional arguments list.
#' @param t_min (scalar | vector | column matrix) the lower bound of the interval for each sampled point process
#'        The length of this argument is the number of point processes that should be drawn.
#' @param t_max (scalar | vector | column matrix) the upper bound of the interval for each sampled point process
#'        The length of this argument is the number of point processes that should be drawn.
#' @param Lambda_args (list) optional arguments to pass to Lambda.
#' @param Lambda_inv_args (list) optional arguments to pass to Lambda_inv().
#' @param tol the tolerange for the calulations.
#' @param atmost1 boolean, draw at most 1 event time per sampled point process.
#' @param atleast1 boolean, draw at least 1 event time
#'
#' @return a matrix of event times with one row per sampled point process.
#' @export
#'
vdraw_cumulative_intensity <- function(Lambda,
                                       Lambda_inv,
                                       t_min,
                                       t_max,
                                       Lambda_args = NULL,
                                       Lambda_inv_args = NULL,
                                       tol = 10^-6,
                                       atmost1 = FALSE,
                                       atleast1 = FALSE) {
  if (atleast1 == TRUE) {
    stop("Option `atleast1 = TRUE` has not been implemented yet for vectorized functions.")
  }
  range_t <- cbind(as.vector(t_min), as.vector(t_max))
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
