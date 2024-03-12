#include "nhppp.h"

// [[Rcpp::export]]
void matrix_cumsum_columns_inplace(Rcpp::NumericMatrix & X) {
  for(int c = 1; c!=X.cols(); ++c){
    X(Rcpp::_,c) = X(Rcpp::_,c-1) + X(Rcpp::_,c);
  }
}
