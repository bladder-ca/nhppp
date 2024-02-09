#include "nhppp.h"

// [[Rcpp::export]]
void matrix_diff_columns_inplace(Rcpp::NumericMatrix & X) {
  for(int c = X.cols() - 1; c != 0 ; --c){
    X(Rcpp::_,c) = X(Rcpp::_,c) - X(Rcpp::_,c-1);
  }
}
