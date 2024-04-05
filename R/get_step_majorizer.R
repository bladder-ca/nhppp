#' Piecewise constant (step) majorizer for K-Lipschitz functions over an interval
#' (vectorized over the `breaks` argument).
#'
#' @description Return a piecewise constant (step) majorizer for K-Lipschitz functions
#'              over an interval. The function is vectorized over the `breaks` argument.
#' @param fun A function object with a single argument `x`.
#'            If `x` is a matrix, `fun` should be vectorized over it.
#' @param breaks (vector or matrix) The set of `M+1` boundaries for the `M` subintervals in `x`.
#'               If breaks is a matrix, each row is treated as a separate set of breaks.
#' @param is_monotone (boolean) Is the function monotone? (Default is `TRUE`.)
#' @param K (double) A non-negative number for the Lipschitz cone. (Default is 0.)
#' @return A vector of length `M` with the values of the piecewise constant majorizer
#'
#' @export
#' @examples
#' get_step_majorizer(fun = abs, breaks = -5:5, is_monotone = FALSE, K = 1)
get_step_majorizer <- function(fun, breaks, is_monotone = TRUE, K = 0) {
  # browser()
  if (K < 0) stop()
  if (is.vector(breaks)) {
    breaks <- matrix(breaks, nrow = 1)
  }
  M <- ncol(breaks) - 1
  f_breaks <- fun(breaks)

  lambda_star <- pmax(f_breaks[, 1:M], f_breaks[, 2:(M + 1)])
  if (isTRUE(is_monotone)) {
    return(lambda_star)
  } else {
    return(lambda_star + K * abs(breaks[, 1:M] - breaks[, 2:(M + 1)]) / 2)
  }
}
