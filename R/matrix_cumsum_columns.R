# Usage: matrix_cumsum_columns( X )
matrix_cumsum_columns <- function(X) {
  .Call(`_nhppp_matrix_cumsum_columns`, X)
}
