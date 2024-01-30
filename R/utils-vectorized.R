#' Return matrix with column-wise cumulative sum
#' No checks for arguments is done.
#'
#' @param X (matrix)
#' @return matrix
mat_cumsum_columns <- function(X) {
  if (ncol(X) > 1) {
    return(t(apply(X, 1, cumsum)))
  } else {
    return(X)
  }
}

#' Return matrix with column-wise cumulative sum
#' replacing cells larger than `ceil` with `NA`.
#' No checks for arguments is done.
#'
#' @param X (matrix)
#' @param ceil (double or Inf) the ceiling to be applied
#' @return matrix
mat_cumsum_columns_with_scalar_ceiling <- function(X, ceil = Inf) {
  X <- mat_cumsum_columns(X)

  if (is.infinite(ceil)) {
    return(X)
  }

  X[which(X > ceil)] <- NA
  for (col in 1:ncol(X)) {
    if (all(is.na(X[, col]))) {
      if (col > 2) {
        return(X[, 1:(col - 1), drop = FALSE])
      } else {
        return(X[, 1, drop = FALSE])
      }
    }
  }
  return(X)
}


#' Return matrix with column-wise cumulative sum
#' replacing cells larger than `ceil` with `NA`.
#' No checks for arguments is done.
#'
#' @param X (matrix)
#' @param ceil (vector or Inf) the set of ceilings to be applied, per row of `X`
#' @return matrix
mat_cumsum_columns_with_vector_ceiling <- function(X, ceil = Inf) {
  # browser()
  X <- mat_cumsum_columns(X)
  if (all(is.infinite(ceil))) {
    return(X)
  }
  for (col in 1:ncol(X)) {
    X[, col] <- pmin(X[, col], ceil)
    X[X[, col] == ceil, col] <- NA
    if (all(is.na(X[, col]))) {
      if (col > 2) {
        return(X[, 1:(col - 1), drop = FALSE])
      } else {
        return(X[, 1, drop = FALSE])
      }
    }
  }
  return(X)
}
