#include <string>
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
  const bool atmost1, 
  const int atmostB) {


  int n_intervals = rate_maj.cols();
  int n_draws = rate_maj.rows();
  NumericVector interval_duration = (range_t(_,1) - range_t(_,0))/n_intervals;
  NumericMatrix Lambda_maj(n_draws, n_intervals);
  NumericMatrix lambda_maj(n_draws, n_intervals);


  if(!is_cumulative) {
    lambda_maj = rate_maj;
    Lambda_maj = matrix_cumsum_columns(rate_maj);
    for(int i = 0; i!= n_intervals; ++i){
      Lambda_maj.column(i) = Lambda_maj.column(i) * interval_duration;
    }
  } else {
    Lambda_maj = rate_maj;
    lambda_maj = matrix_diff_columns(rate_maj);
    for(int i = 0; i!= n_intervals; ++i){
      lambda_maj.column(i) = lambda_maj.column(i) / interval_duration;
    }
  }

  NumericMatrix Zstar;//(n_draws, n_intervals);
  if(use_subinteval) {
    Zstar = vdraw_sc_step_regular2(Lambda_maj, true, range_t, subinterval, tol, false, atmostB);
  } else {
    Zstar = vdraw_sc_step_regular(Lambda_maj, true, range_t, tol, false);
  }
  

  //bool accept;
  double acceptance_prob;
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
      //interval = floor(Zstar(draw, ev) / interval_duration(draw));
      interval = floor(Zstar(draw, ev) - range_t(draw, 0)) / interval_duration(draw);
      acceptance_prob = (lambda_star(draw, ev)/lambda_maj(draw, interval));
      if(acceptance_prob > 1 || acceptance_prob < 0) {
        // double zs = Zstar(draw, ev); 
        // double ls = lambda_star(draw, ev); 
        // double lm = lambda_maj(draw, ev); 
        // double lm1 = lambda_maj(draw, interval); 
        // double Lm = Lambda_maj(draw, ev); 
        // double Lm1 = Lambda_maj(draw, interval);  
        // double rm = rate_maj(draw, ev);
        // double rm1 = rate_maj(draw, interval);

        std::string str = "Majorizer error? Pr(acceptance) = ";
        str += std::to_string(acceptance_prob);
        throw std::range_error(str);
      }
      
      
      if(acceptance_prob > (R::runif(0.0, 1.0))) {
        Z(draw,acc_i) = Zstar(draw, ev);
        max_acc_i = std::max(max_acc_i, acc_i);
        ++acc_i;
        if(atmost1) {
          break;
        }
      }
    }
  }

  

  return Z(Range(0, n_draws-1), Range(0, max_acc_i));


}
