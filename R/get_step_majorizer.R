#' Piecewise constant (step) majorizer for K-Lipschitz functions over an interval
#' (vectorized over the `breaks` argument).
#'
#' @description Return a piecewise constant (step) majorizer for K-Lipschitz functions
#'              over an interval. The function is vectorized over the `breaks` argument.
#'              The returned object has the same dimensions as `breaks`.
#' @param fun A function object with a single argument `x`.
#'            If `x` is a matrix, `fun` should be vectorized over it.
#' @param breaks (vector or matrix) The set of `M+1` boundaries for the `M` subintervals in `x`.
#'               If breaks is a matrix, each row is treated as a separate set of breaks.
#' @param is_monotone (boolean) Is the function monotone? (Default is `TRUE`.)
#' @param K (double) A non-negative number for the Lipschitz cone. (Default is 0.)
#' @param fun_args (list) arguments for `fun`, with up to two elements:
#'        `shared` (named list of row-invariant arguments) and `row_args`
#'        (data.frame or data.table with one row per row of `breaks`). When
#'        non-`NULL`, the container is passed as `fun`'s second positional
#'        argument — the same convention (and the same `lambda`) as the
#'        vectorized samplers.
#' @return A vector of length `M` with the values of the piecewise constant majorizer
#'
#' @export
#' @examples
#' get_step_majorizer(fun = abs, breaks = -5:5, is_monotone = FALSE, K = 1)
get_step_majorizer <- function(fun, breaks, is_monotone = TRUE, K = 0, fun_args = NULL) {
  # browser()
  if (K < 0) stop()
  drop <- FALSE
  if (is.vector(breaks)) {
    breaks <- matrix(breaks, nrow = 1)
    drop <- TRUE
  }
  M <- ncol(breaks) - 1
  fa_ <- .resolve_fun_args(fun_args, n_draws = nrow(breaks), arg_name = "fun_args")
  f_breaks <- .call_with_args(fun, breaks, fa_$container)

  lambda_star <- pmax(f_breaks[, 1:M, drop = drop], f_breaks[, 2:(M + 1), drop = drop])
  if (isTRUE(is_monotone)) {
    return(lambda_star)
  } else {
    return(lambda_star + K * abs(breaks[, 1:M, drop = drop] - breaks[, 2:(M + 1), drop = drop]) / 2)
  }
}
