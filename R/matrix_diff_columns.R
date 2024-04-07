# Usage: matrix_diff_columns( X )
matrix_diff_columns <- function(X) {
  .Call(`_nhppp_matrix_diff_columns`, X)
}
