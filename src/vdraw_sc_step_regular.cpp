#include "nhppp.h"
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix vdraw_sc_step_regular(
  const NumericMatrix & rate,
  const bool is_cumulative,
  const NumericMatrix & range_t,
  const double tol,
  const bool atmost1
) {
  int n_intervals = rate.cols();
  int n_draws = rate.rows();  
  NumericVector interval_duration = (range_t(_,1) - range_t(_,0))/n_intervals;
  NumericMatrix Lambda(n_draws, n_intervals);
  if(!is_cumulative) {
    Lambda = matrix_cumsum_columns(rate);
    for(int i = 0; i!= n_intervals; ++i){
      Lambda.column(i) = Lambda.column(i) * interval_duration; 
    }
  } else {
    Lambda = rate;
  }

  int n_max_events = safe_double_to_int(R::qpois(1.0 - tol, max(Lambda), 1, 0));
  if(n_max_events == 0) {
    NumericMatrix Z(n_draws, 1); 
    std::fill( Z.begin(), Z.end(), NumericVector::get_na() ) ;
    return(Z);
  }
  n_max_events = (atmost1)?1:n_max_events;

  NumericMatrix Tau(n_draws, n_max_events);
  for(int i =0; i!=n_draws*n_max_events; ++i) {
    Tau[i] = R::rexp(1);
  }
  if(n_max_events>1){
    matrix_cumsum_columns_inplace(Tau);  
  }
 

  return step_regular_inverse(n_max_events, Lambda, Tau, range_t, atmost1);
}


