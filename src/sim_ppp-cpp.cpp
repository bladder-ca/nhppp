#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector sim_ppp_cn(
  const double rate, 
  const int n, 
  const double t_min
  ){
  NumericVector diff_times(n), times(n);
  diff_times = rexp(n, rate);
    std::partial_sum(diff_times.begin(), diff_times.end(), times.begin());
  return times + t_min;
}


// [[Rcpp::export]]
NumericVector sim_ppp_ct(
  const double rate,  
  const double t_min, 
  const double t_max, 
  const double tol = 0.000001
  ){
  int n;

  n = qpois(1.0 - tol, rate * (t_max - t_min)); 
  
  NumericVector times(n); 
  times = sim_ppp_cn(rate, n, t_min); 

  return times[times <= t_max];
}



