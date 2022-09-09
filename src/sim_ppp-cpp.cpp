#include <Rcpp.h>
#include <Rmath.h>
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
  const double tol = 10E-6){
  int n;

  n = R::qpois(1.0 - tol, rate * (t_max - t_min), TRUE, FALSE); 
  
  NumericVector times(n); 
  times = sim_ppp_cn(rate, n, t_min); 

  return times[times <= t_max];
}


// [[Rcpp::export]]
double big_lambda(double t){
  return (t*t);
}

// [[Rcpp::export]]
double big_lambda_inv(double z){
  return (sqrt(z));
}


// [[Rcpp::export]]
int add(int x, int y)
{
    return x + y;
}
  
// [[Rcpp::export]]
int multiply(int x, int y)
{
    return x * y;
}
  

// [[Rcpp::export]]
int invoke(int x, int y,
           int (*func)(int, int))
{
    return func(x, y);
}

/*
NumericVector sim_nhppp_ct_inv(
  double (*big_lambda_ptr)(double),
  double (*big_lambda_inv_ptr)(double),
  const double t_min, 
  const double t_max){

  NumericVector dat_warped_time; 

  dat_warped_time = sim_ppp_ct(1, (*big_lambda_ptr)(t_min), (*big_lambda_ptr)(t_max));

  if (dat_warped_time.size() == 0) {
    return dat_warped_time;
  } else {
    return (*big_lambda_inv_ptr)(dat_warped_time);
  }
}
*/





