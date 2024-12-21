#include "nhppp.h"

// [[Rcpp::export]]
int rztpois(const double lambda){
  double p = R::runif(exp(-lambda), 1.0); 
  return safe_double_to_int(R::qpois(p, lambda, 1, 0));
}

// [[Rcpp::export]]
Rcpp::IntegerVector rztpois_vec(const Rcpp::NumericVector & lambda) {
  Rcpp::IntegerVector n (lambda.size()); 
  for(int i = 0; i != lambda.size(); ++i){
    n[i] = rztpois(lambda[i]);
  }
  return n;
}


int safe_double_to_int(const double x){
  if(x > std::numeric_limits<int>::max()){
    return std::numeric_limits<int>::max();
  } else if(x < std::numeric_limits<int>::min()){
    return std::numeric_limits<int>::min();
  }
  return static_cast<int>(x);
}
