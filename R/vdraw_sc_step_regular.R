#' Vectorized sampling from NHPPPs with piecewise constant intensities
#' with same interval lengths (R)
#'
#' Simulate a piecewise constant-rate Poisson Point Process over `(t_min, t_max]` (inversion method)
#' where the intervals have the same length (are "regular").
#'
#' @param Lambda_matrix (matrix) integrated intensity rates at the end of each interval
#' @param lambda_matrix (matrix) intensity rates, one per interval
#' @param range_t (vector, double) `t_min` and `t_max`
#' @param tol (scalar, double) tolerance for the number of events
#' @param atmost1 boolean, draw at most 1 event time
#' @param atleast1 boolean, draw at least 1 event time (zero-truncated NHPPP)
#' @param use_cpp (boolean, TRUE) use the C++ implementation of the function
#'
#' @return a vector of event times t
#'         if no events realize, it will have 0 length
#' @export
#'
#' @examples
#' x <- vdraw_sc_step_regular(Lambda_matrix = matrix(1:5, nrow = 1))
vdraw_sc_step_regular <- function(Lambda_matrix = NULL,
                                  lambda_matrix = NULL,
                                  range_t = c(0, 10),
                                  tol = 10^-6,
                                  atmost1 = FALSE,
                                  atleast1 = FALSE,
                                  use_cpp = TRUE) {
  if (isTRUE(use_cpp) && isFALSE(atleast1)) {
    return(
      vdraw_sc_step_regular_cpp(
        Lambda_matrix = Lambda_matrix,
        lambda_matrix = lambda_matrix,
        range_t = range_t,
        tol = tol,
        atmost1 = atmost1
      )
    )
  } else if (isTRUE(use_cpp) && isTRUE(atleast1)) {
    return(
      vztdraw_sc_step_regular_cpp(
        Lambda_matrix = Lambda_matrix,
        lambda_matrix = lambda_matrix,
        range_t = range_t,
        atmost1 = atmost1
      )
    )
  } else if (isFALSE(use_cpp) && isFALSE(atleast1)) {
    return(
      vdraw_sc_step_regular_cpp(
        Lambda_matrix = Lambda_matrix,
        lambda_matrix = lambda_matrix,
        range_t = range_t,
        tol = tol,
        atmost1 = atmost1
      )
    )
  } else if (isFALSE(use_cpp) && isTRUE(atleast1)) {
    return(
      vztdraw_sc_step_regular_cpp(
        Lambda_matrix = Lambda_matrix,
        lambda_matrix = lambda_matrix,
        range_t = range_t,
        atmost1 = atmost1
      )
    )
  } else {
    stop("never here")
  }
}
