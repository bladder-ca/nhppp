#' Simpson's method to integrate a univariate function.
#'
#' @description Simpson's method to integrate a univariate continuous function.
#' Faster that R's `integrate()` and precise enough, but does not do any checks.
#' The error is at most `M (b-a)^5/(180 n^4)` where `M` is the maximum of
#' the fourth derivative of the integrand in the interval `[a, b]`.
#' @param f function that takes a single argument
#' @param a the lower limit of integration
#' @param b the upper limit of integration
#' @param n integer, number of integration points with a and b
#' @return numeric, the integration value
#' examples
#' #expect 1
#' simpson_num_integr(sin, 0, pi/2, 100)
#' #max error for simpson_num_integr(sin, 0, pi/2, 100) is 5.312842e-10
#' 1 * (pi/2 - 0)^5/(180 * 100^4)
#' @export
simpson_num_integr <- function(f, a, b, n) {
  if (is.function(f) == FALSE) {
    stop("f must be a function with one parameter (variable)")
  }
  h <- (b - a) / n
  xj <- seq.int(a, b, length.out = n + 1)
  xj <- xj[-1]
  xj <- xj[-length(xj)]
  integral <- (h / 3) *
    (f(a) +
      2 * sum(f(xj[seq.int(2, length(xj), 2)])) +
      4 * sum(f(xj[seq.int(1, length(xj), 2)])) +
      f(b))
  return(integral)
}
