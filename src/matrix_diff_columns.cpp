#include "nhppp.h"

// [[Rcpp::export]]
Rcpp::NumericMatrix matrix_diff_columns(const Rcpp::NumericMatrix & X) {
  Rcpp::NumericMatrix Y(X.rows(), X.cols()); 
  std::copy(X.begin(), X.end(), Y.begin());
  matrix_diff_columns_inplace(Y);
  return Y;
}
