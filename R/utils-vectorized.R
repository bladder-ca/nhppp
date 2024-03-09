#' Helper function for the vectorized versions of sampling functions.
#' Takes the usual ways that `range_t` is specified
#' (a 2-vector, a 1 x 2 or an r x 2 matrix) and
#' returns a r x 2 matrix.
#'
#' @param range_t a 2-vector, a 1 x 2 or an r x 2 matrix
#' @param n_rows the number of rows in the fully expanded matrix (`r`)
#' @return A matrix (r x 2), row-expanded if needed
make_range_t_matrix <- function(range_t, n_rows) {
  if (is.matrix(range_t) && nrow(range_t) == n_rows && ncol(range_t) == 2) {
    return(range_t)
  }
  if (is.matrix(range_t) && nrow(range_t) == 1 && ncol(range_t) == 2) {
    return(range_t[rep(1, n_rows), ])
  }
  if (is.vector(range_t) && length(range_t) == 2) {
    return(matrix(rep(range_t, each = n_rows), ncol = 2))
  }
  stop("`range_t` is not a 2-vector, a 1 x 2 or an `r x 2` matrix")
}


#' Helper function for the vectorized versions of sampling functions.
#' Takes the usual ways that `lambda_mat` and `Lambda_mat` are specified
#' and returns `lambda_mat`.
#'
#' @param lambda_mat a matrix of intensities or missing
#' @param Lambda_mat a matrix of cumulative intensities or missing
#' @param interval_duration a vector with the same number of elements as the rows of `Lambda_mat`
#' @return A matrix (r x 2), row-expanded if needed
make_lambda_matrix <- function(lambda_mat = NULL, Lambda_mat = NULL, interval_duration = NULL) {
  if (!is.null(lambda_mat)) {
    mode(lambda_mat) <- "numeric"
    return(lambda_mat)
  }
  if (!is.null(Lambda_mat) && !is.null(interval_duration)) {
    mode(Lambda_mat) <- "numeric"
    lambda_mat <- mat_diff_columns(Lambda_mat) / interval_duration
    return(lambda_mat)
  }
  stop("missing argument (`interval_duration` is needed with `Lambda_mat`")
}


#' Helper function for the vectorized versions of sampling functions.
#' Takes the usual ways that `lambda_mat` and `Lambda_mat` are specified
#' and returns `Lambda_mat`.
#'
#' @param lambda_mat a matrix of intensities or missing
#' @param Lambda_mat a matrix of cumulative intensities or missing
#' @param interval_duration a vector with the same number of elements as the rows of `Lambda_mat`
#' @return A matrix (r x 2), row-expanded if needed
make_cumulative_Lambda_matrix <- function(Lambda_mat = NULL, lambda_mat = NULL, interval_duration = NULL) {
  if (!is.null(Lambda_mat)) {
    mode(Lambda_mat) <- "numeric"
    return(Lambda_mat)
  }
  if (!is.null(lambda_mat) && !is.null(interval_duration)) {
    mode(lambda_mat) <- "numeric"
    Lambda_mat <- mat_cumsum_columns(lambda_mat) * interval_duration
    return(Lambda_mat)
  }
  stop("missing argument (`interval_duration` is needed with `lambda_mat`")
}


#' Return matrix with column-wise differencing.
#' No checks for arguments is done.
#'
#' @param X (matrix)
#' @return matrix
mat_diff_columns <- function(X) {
  if (ncol(X) > 1) {
    X <- cbind(rep(0, nrow(X)), X)
    return(t(apply(X, 1, diff)))
  } else {
    return(X)
  }
}


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
