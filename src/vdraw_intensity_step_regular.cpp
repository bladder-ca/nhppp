#include "nhppp.h"
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix vdraw_intensity_step_regular(
  const Function & lambda,
  const NumericMatrix & rate_maj,
  const bool is_cumulative,
  const NumericMatrix & range_t,
  const NumericMatrix & subinterval,
  const bool use_subinteval,
  const double tol,
  const bool atmost1) {


  int n_intervals = rate_maj.cols();
  int n_draws = rate_maj.rows();
  NumericVector interval_duration = (range_t(_,1) - range_t(_,0))/n_intervals;
  NumericMatrix Lambda_maj(n_draws, n_intervals);
  NumericMatrix lambda_maj(n_draws, n_intervals);


  if(!is_cumulative) {
    Lambda_maj = rate_maj;
    Lambda_maj = matrix_cumsum_columns(rate_maj);
    for(int i = 0; i!= n_intervals; ++i){
      Lambda_maj.column(i) = Lambda_maj.column(i) * interval_duration;
    }
  } else {
    lambda_maj = matrix_diff_columns(rate_maj);
    for(int i = 0; i!= n_intervals; ++i){
      lambda_maj.column(i) = lambda_maj.column(i) / interval_duration;
    }
    Lambda_maj = rate_maj;
  }

  NumericMatrix Zstar;//(n_draws, n_intervals);
  if(use_subinteval) {
    Zstar = vdraw_sc_step_regular2(Lambda_maj, true, range_t, subinterval, tol, false);
  } else {
    Zstar = vdraw_sc_step_regular(Lambda_maj, true, range_t, tol, false);
  }
  

  bool accept;
  int interval;
  int acc_i = 0;
  int max_acc_i = 0;

  NumericMatrix lambda_star = lambda(Zstar);



  NumericMatrix Z(n_draws, Zstar.cols());
  std::fill(Z.begin(), Z.end(), NumericVector::get_na());


  for(int draw = 0; draw != n_draws; ++draw){
    acc_i = 0;
    for(int ev = 0; ev != Zstar.cols(); ++ev){
      if(NumericVector::is_na(Zstar(draw, ev))) {
        break;
      }
      interval = floor(Zstar(draw, ev) / interval_duration(draw));
      accept = ((lambda_star(draw, ev)/lambda_maj(draw, interval)) > R::runif(0.0, 1.0));
      if(accept) {
        Z(draw,acc_i) = Zstar(draw, ev);
        max_acc_i = std::max(acc_i, max_acc_i);
        ++acc_i;
        if(atmost1) {
          break;
        }
      }
    }
  }

  

  return Z(Range(0, n_draws-1), Range(0, max_acc_i));


}
