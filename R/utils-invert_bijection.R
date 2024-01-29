#' Numerically evaluate the inverse of a function at a specific point
#'
#' @param f (function) the function to be inverted; must be continuous
#'            and increasing
#' @param y (scalar, double) the f(x)=y value in which to evaluate the inverse
#' @param min_x (scalar, double) the min of the domain of f()
#' @param max_x (scalar, double) the max of the domain of f()
#' @param min_y (scalar, double) the min in the range of f()
#' @param max_y (scalar, double) the max in the range of f()
#'
#' @return (scalar, double) vector of x=f^(-1)(y): the inverted value
#' @export
#'
#' @examples
#' inverse_with_uniroot(f = function(x) {
#'   2 * x
#' }, y = 0.5)
inverse_with_uniroot <- function(f = f, y,
                                 min_x = 0, max_x = 1,
                                 min_y = f(min_x), max_y = f(max_x)) {
  x_star <- stats::uniroot((function(x) f(x) - y),
    lower = min_x,
    upper = max_x,
    f.lower = min_y - y,
    f.upper = max_y - y
  )$root
  return(x_star)
}


#' Numerically evaluate the inverse of a monotonically increasing continuous
#' function from R to R at specific points.
#'
#' @param f (function) the function to be inverted; must be continuous
#'            and increasing
#' @param y (vector, double) the f(x)=y values in which to evaluate the inverse;
#'          must be in ascending order
#' @param range_x (vector, double) the min and max of the domain of f()
#' @param range_y (vector, double) the min and max in the range of f()
#'
#' @return (vector, double) vector of x=f^(-1)(y): the inverted values
#' @export
#'
#' @examples
#' inverse_with_uniroot_sorted(f = function(x) {
#'   2 * x
#' }, y = c(0, 0.5))
inverse_with_uniroot_sorted <- function(f, y,
                                        range_x = c(0, 10),
                                        range_y = c(f(range_x[1]), f(range_x[2]))) {
  len <- length(y)
  if (y[1] < range_y[1] || y[len] > range_y[2]) stop()

  x <- rep(NA, len)
  x[1] <- inverse_with_uniroot(
    f = f, y[1],
    min_x = range_x[1], max_x = range_x[2],
    min_y = range_y[1], max_y = range_y[2]
  )

  if (len == 1) {
    return(x)
  }

  for (i in 2:len) {
    x[i] <- inverse_with_uniroot(
      f = f, y[i],
      min_x = x[i - 1], max_x = range_x[2],
      min_y = y[i - 1], max_y = range_y[2]
    )
  }
  return(x)
}
