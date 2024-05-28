#' Vectorized sampling from zero-truncated NHPPPs with piecewise constant intensities
#' with same interval lengths
#'
#' Simulate a piecewise constant-rate Poisson Point Process over `(t_min, t_max]` (inversion method)
#' where the intervals have the same length (are "regular").
#'
#' @param Lambda_matrix (matrix) integrated intensity rates at the end of each interval
#' @param lambda_matrix (matrix) intensity rates, one per interval
#' @param range_t (vector, double) `t_min` and `t_max`
#' @param subinterval (vector, double) optional -- the subinterval of `range_t` to sample. If `NULL`, the whole range_t is used.
#' @param atmost1 boolean, draw at most 1 event time
#' @param ... (any) other arguments (ignored  -- used for flexibility in calling from other functions)
#'
#' @return a vector of event times t
#'         if no events realize, it will have 0 length
#' @export
#'
#' @examples
#' x <- vztdraw_sc_step_regular(Lambda_matrix = matrix(1:5, nrow = 1))
#' @export
vztdraw_sc_step_regular <- function(Lambda_matrix = NULL,
                                    lambda_matrix = NULL,
                                    range_t = c(0, 10),
                                    subinterval = NULL,
                                    atmost1 = FALSE,
                                    ...) {
  return(
    vztdraw_sc_step_regular_cpp(
      Lambda_matrix = Lambda_matrix,
      lambda_matrix = lambda_matrix,
      range_t = range_t,
      subinterval = subinterval,
      atmost1 = atmost1,
      ...
    )
  )
}
