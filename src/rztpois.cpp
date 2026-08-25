#include "nhppp.h"

// K-truncated Poisson: N ~ Poisson(lambda) conditional on N >= k, by
// inversion of the CDF restricted to the truncation region.
// k == 1 uses the closed form F(0) = exp(-lambda) on the lower tail;
// k >= 2 samples the upper tail directly (p ~ U(0, P(N >= k)), then the
// upper-tail quantile), which stays accurate when P(N >= k) is tiny.
int rbtpois(const double lambda, const int k){
  if (k <= 1) {
    double p = R::runif(exp(-lambda), 1.0);
    return safe_double_to_int(R::qpois(p, lambda, 1, 0));
  }
  const double tail = R::ppois(k - 1, lambda, 0, 0); // P(N >= k)
  const double p = R::runif(0.0, tail);
  return safe_double_to_int(R::qpois(p, lambda, 0, 0));
}

// [[Rcpp::export]]
Rcpp::IntegerVector rbtpois_vec(const Rcpp::NumericVector & lambda, const int k) {
  Rcpp::IntegerVector n (lambda.size());
  for(int i = 0; i != lambda.size(); ++i){
    n[i] = rbtpois(lambda[i], k);
  }
  return n;
}

// [[Rcpp::export]]
int rztpois(const double lambda){
  return rbtpois(lambda, 1);
}

// [[Rcpp::export]]
Rcpp::IntegerVector rztpois_vec(const Rcpp::NumericVector & lambda) {
  return rbtpois_vec(lambda, 1);
}


int safe_double_to_int(const double x){
  if(x > std::numeric_limits<int>::max()){
    return std::numeric_limits<int>::max();
  } else if(x < std::numeric_limits<int>::min()){
    return std::numeric_limits<int>::min();
  }
  return static_cast<int>(x);
}
