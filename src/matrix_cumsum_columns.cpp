
#include "nhppp.h"

// [[Rcpp::export]]
Rcpp::NumericMatrix matrix_cumsum_columns(const Rcpp::NumericMatrix & X) {
  int n_col = X.cols();
  int n_rows = X.rows();

  Rcpp::NumericMatrix Y(n_rows, n_col); 
  std::copy(X.column(0).begin(), X.column(0).end(), Y.column(0).begin());
  if(n_col == 1) {
  	return Y;
  }
  for(int c = 1; c != n_col; ++c){
  	Y(Rcpp::_,c) = Y(Rcpp::_,c-1) + X(Rcpp::_,c);
  }
  return Y;
}


