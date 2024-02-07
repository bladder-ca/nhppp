#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
void matrix_diff_columns_inplace(NumericMatrix & X) {
  for(int c = X.cols() - 1; c != 0 ; --c){
    X(_,c) = X(_,c) - X(_,c-1);
  }
}
