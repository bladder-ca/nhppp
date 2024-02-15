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
#'
#' @return a matrix of event times (columns) per draw (rows)
#'         NAs are structural empty spots
#' @export
#'
#' @examples
#' Z <- vztdraw_intensity_step_regular(
#'  lambda = function(x, ...) 0.1 * x,
#'  lambda_maj_matrix = matrix(rep(1, 5), nrow = 1)
#'  )
#' @export
vztdraw_intensity_step_regular <- function(lambda,
                                           lambda_args = NULL,
                                           Lambda_maj_matrix = NULL,
                                           lambda_maj_matrix = NULL,
                                           range_t = c(0, 10),
                                           tol = 10^-6,
                                           atmost1 = FALSE) {


  Z <- vdraw_intensity_step_regular(lambda = lambda,
                                           lambda_args = lambda_args,
                                           Lambda_maj_matrix = Lambda_maj_matrix,
                                           lambda_maj_matrix = lambda_maj_matrix,
                                           range_t = range_t,
                                           tol = tol,
                                           atmost1 = atmost1,
                                           force_zt = TRUE)

  has_no_times <- is.na(Z[,1])
  max_events <- ncol(Z)

  while(sum(has_no_times)>0) {

    Z_add <- vdraw_intensity_step_regular(lambda = lambda,
                                         lambda_args = lambda_args,
                                         Lambda_maj_matrix = Lambda_maj_matrix[has_no_times,],
                                         lambda_maj_matrix = lambda_maj_matrix[has_no_times,],
                                         range_t = range_t[has_no_times,],
                                         tol = tol,
                                         atmost1 = atmost1,
                                         force_zt = TRUE)

    diff_cols <- ncol(Z_add) - ncol(Z)
    if(diff_cols>0) {
      for(co in 1:diff_cols) {
        Z <- cbind(Z, rep(NA, nrow(Z)))
      }
    } else if (diff_cols < 0) {
      for(co in 1:abs(diff_cols)) {
        Z_add <- cbind(Z_add, rep(NA, sum(has_no_times)))
      }
    } else {
      Z_add
    }
    #browser()
    Z[has_no_times,] <- Z_add

    has_no_times <- is.na(Z[,1])

  }

  return(Z)
}
