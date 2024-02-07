#include "nhppp.h"

// [[Rcpp::export]]
int rztpois(const double lambda){
  double p = R::runif(exp(-lambda), 1.0); 
  return R::qpois(p, lambda, 1, 0);
}

// [[Rcpp::export]]
Rcpp::IntegerVector rztpois_vec(const Rcpp::NumericVector & lambda) {
  Rcpp::IntegerVector n (lambda.size()); 
  for(int i = 0; i != lambda.size(); ++i){
    n[i] = rztpois(lambda[i]);
  }
  return n;
}
