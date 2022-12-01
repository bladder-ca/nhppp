#include "nhppp_types.h"
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
  const double tol = 10E-6, 
  const bool only1 = false){
  
  int n;
  if(only1 == true ){
    n = 1;
  } else {
    n = R::qpois(1.0 - tol, rate * (t_max - t_min), TRUE, FALSE); 
  }

  NumericVector times(n); 
  times = sim_ppp_cn(rate, n, t_min); 

  return times[times <= t_max];
}


// [[Rcpp::export]]
NumericVector sim_nhppp_ct_inv(
  const double t_min, 
  const double t_max,
  std::string L_str  = "L_FAIL",
  NumericVector L_params = (0.0),
  std::string L_inv_str  = "Linv_FAIL", 
  NumericVector L_inv_params = (0.0),
  bool only1 = false){


  NumericVector dat_warped_time; 
  XPtr<funcPtr> xp_L = putFunPtrInXPtr2(L_str);
  funcPtr L = *xp_L;

  XPtr<funcPtr> xp_L_inv = putFunPtrInXPtr2(L_inv_str);
  funcPtr L_inv = *xp_L_inv;

  if(only1) {
    dat_warped_time = sim_ppp_cn(1, 1, L(t_min, L_params));
    dat_warped_time[0] = L_inv(dat_warped_time[0], L_inv_params);
    return (dat_warped_time);      
  } 

  dat_warped_time = sim_ppp_ct(1, L(t_min, L_params), L(t_max, L_inv_params));
  int n = dat_warped_time.size();
  if (n == 0) {
    return dat_warped_time;
  } else {
    for(int i =0; i<n; i++){
      dat_warped_time[i] = L_inv(dat_warped_time[i], L_inv_params);
    }
    return (dat_warped_time);  
  }
}

/***R
 sim_nhppp_ct_inv(0, 3, "L", "Linv", FALSE)
*/



// [[Rcpp::export]]
NumericVector sim_nhppp_ct_linear(
  const double alpha,
  const double beta,
  const double t_min, 
  double t_max, 
  const double tol = 1e-6, 
  const bool only1 = false
  ){
  if (beta == 0) {
    if(alpha <=0) { 
      stop("Error: beta == 0 and alpha <= 0 "); 
    }
    return sim_ppp_ct(alpha, t_min, t_max, tol, only1) ;
  }
  if (alpha + beta * t_min < 0) {
    stop("Error: alpha + beta * t_min < 0");
  }
  if (beta < 0) {
    t_max = std::min(-alpha / beta, t_max);
  }
  return sim_nhppp_ct_inv(
      t_min, 
      t_max,
      "L", 
      (alpha, beta, t_min),
      "Linv",
      (alpha, beta, t_min), 
      only1);
}


// [[Rcpp::export]]
NumericVector sim_nhppp_ct_thinning(
  const double t_min, 
  const double t_max,
  const double l_max,
  std::string l_str  = "l_FAIL", 
  bool only1 = false){


  NumericVector params_l = (0.0);
  const double l_star = l_max; 
  

  XPtr<funcPtr> xp_l = putFunPtrInXPtr2(l_str);
  funcPtr lambda = *xp_l;

  // 2 uniforms per while -- second is in thinning
  NumericVector U(2); 
  NumericVector times; 
  
  double t_new = t_min; 
  
  int i = 0;
  while (t_new <= t_max) {
    // draw 2 uniforms per while; second is used in the thinning
    U = runif(2, 0, 1); 
    
    t_new = t_new - log(U[0]) / l_star;
    // the last t_min could go above range_t[2] - catch it here
    if (U[1] < lambda(t_new, params_l) / l_star && t_new <= t_max) {
      times.push_back(t_new);
      i++;
      if (only1 && i == 1) {
        break;
      }
    }
  }
  return(times);
}

/***R
 sim_nhppp_ct_thinning(2, 3, 6, "l", FALSE)
*/

/***R
 microbenchmark::microbenchmark(
   sim_nhppp_ct_inv(0, 10, "L", "Linv", TRUE),
   sim_nhppp_ct_inv(0, 10, "L", "Linv", FALSE)[1],
   sim_nhppp_ct_thinning(0, 10, 20, "l", TRUE),
   sim_nhppp_ct_thinning(0, 10, 20, "l", FALSE)[1], 
   times = 1000
 ) 
*/


