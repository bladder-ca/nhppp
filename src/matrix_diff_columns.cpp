#include "nhppp.h"

// [[Rcpp::export]]
Rcpp::NumericMatrix matrix_diff_columns(const Rcpp::NumericMatrix & X) {
  int n_col = X.cols();
  int n_rows = X.rows();

  Rcpp::NumericMatrix Y(n_rows, n_col); 
  std::copy(X.column(n_col-1).begin(), X.column(n_col-1).end(), Y.column(n_col-1).begin());
  if(n_col == 1) {
    return Y;
  }
  for(int c = n_col - 1; c != 0; --c){
    Y(Rcpp::_,c) = Y(Rcpp::_,c) - X(Rcpp::_,c-1);
  }
  return Y;
}
