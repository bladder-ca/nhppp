#include "nhppp.h"

// Truncated Poisson: N ~ Poisson(lambda) conditional on k_min <= N <= k_max,
// by inversion of the CDF restricted to the truncation region.
// k_max <= 0 means unbounded above; k_min <= 0 means unbounded below.
// The upper-tail parameterization (p ~ U(P(N > k_max), P(N >= k_min)), then
// the upper-tail quantile) stays accurate when the region is deep in the
// right tail; the zero-truncated case keeps the closed form F(0) = e^-lambda.
int rbtpois(const double lambda, const int k_min, const int k_max){
  const bool bounded_above = (k_max > 0);
  if (!bounded_above && k_min == 1) {
    double p = R::runif(exp(-lambda), 1.0);
    return safe_double_to_int(R::qpois(p, lambda, 1, 0));
  }
  const double tail_hi = (k_min >= 1) ? R::ppois(k_min - 1, lambda, 0, 0) : 1.0; // P(N >= k_min)
  const double tail_lo = bounded_above ? R::ppois(k_max, lambda, 0, 0) : 0.0;    // P(N > k_max)
  const double p = R::runif(tail_lo, tail_hi);
  return safe_double_to_int(R::qpois(p, lambda, 0, 0));
}

// [[Rcpp::export]]
Rcpp::IntegerVector rbtpois_vec(const Rcpp::NumericVector & lambda,
                                const int k_min, const int k_max) {
  Rcpp::IntegerVector n (lambda.size());
  for(int i = 0; i != lambda.size(); ++i){
    n[i] = rbtpois(lambda[i], k_min, k_max);
  }
  return n;
}

// [[Rcpp::export]]
int rztpois(const double lambda){
  return rbtpois(lambda, 1, 0);
}

// [[Rcpp::export]]
Rcpp::IntegerVector rztpois_vec(const Rcpp::NumericVector & lambda) {
  return rbtpois_vec(lambda, 1, 0);
}


int safe_double_to_int(const double x){
  if(x > std::numeric_limits<int>::max()){
    return std::numeric_limits<int>::max();
  } else if(x < std::numeric_limits<int>::min()){
    return std::numeric_limits<int>::min();
  }
  return static_cast<int>(x);
}
