#' Vectorized sampling from a non homogeneous Poisson Point Process (NHPPP) from
#'    an interval (thinning method) with piecewise constant_majorizers (C++)
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
#'
#' @return a matrix of event times (columns) per draw (rows)
#'         NAs are structural empty spots
#' @export
#'
#' @examples
#' Z <- vdraw_intensity_step_regular_cpp(
#'  lambda = function(x, ...) 0.1 * x,
#'  lambda_maj_matrix = matrix(rep(1, 5), nrow = 1)
#'  )
#' @export
vdraw_intensity_step_regular_cpp <- function(lambda,
                                             lambda_args = NULL,
                                             Lambda_maj_matrix = NULL,
                                             lambda_maj_matrix = NULL,
                                             range_t = c(0, 10),
                                             tol = 10^-6,
                                             atmost1 = FALSE){
  if(!is.null(lambda_maj_matrix) && is.null(Lambda_maj_matrix)) {
    rate <- lambda_maj_matrix
    is_cumulative_rate <- FALSE
  } else if(is.null(lambda_maj_matrix) && !is.null(Lambda_maj_matrix)) {
   rate <- Lambda_maj_matrix
   is_cumulative_rate <- TRUE
  } else {
    stop("lambda_maj_matrix and Lambda_maj_matrix cannot both be `NULL`")
  }
  if(!is.matrix(range_t)) {
    range_t = matrix(rep(range_t, each = nrow(rate)), ncol = 2)
  } else if(all.equal(dim(range_t), c(1,2))) {
    range_t <- range_t[rep(1, nrow(rate)),]
  } else {
    stop("`range_t` should have as many rows as `lambda_maj_matrix` or `Lambda_maj_matrix`")
  }

  mode(rate) <- "numeric"

  return(
    .Call(`_nhppp_vdraw_intensity_step_regular`, lambda,
          rate, is_cumulative_rate, range_t, tol, atmost1)
  )
}
