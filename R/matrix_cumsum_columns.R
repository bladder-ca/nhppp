#' Usage: matrix_cumsum_columns( X )
#' @keywords internal
matrix_cumsum_columns <- function(X) {
  .Call(`_nhppp_matrix_cumsum_columns`, X)
}
