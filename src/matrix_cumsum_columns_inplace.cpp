#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
void matrix_cumsum_columns_inplace(NumericMatrix & X) {
  for(int c = 1; c!=X.cols(); ++c){
    X(_,c) = X(_,c-1) + X(_,c);
  }
}
