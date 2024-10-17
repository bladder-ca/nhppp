#' Usage: matrix_diff_columns_inplace( X )
#' @keywords internal
matrix_diff_columns_inplace <- function(X) {
  .Call(`_nhppp_matrix_diff_columns_inplace`, X)
  invisible(NULL)
}
