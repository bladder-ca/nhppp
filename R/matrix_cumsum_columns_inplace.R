# Usage: matrix_cumsum_columns_inplace( X )
matrix_cumsum_columns_inplace <- function(X) {
  .Call(`_nhppp_matrix_cumsum_columns_inplace`, X)
  invisible(NULL)
}
