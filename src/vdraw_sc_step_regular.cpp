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
  NumericMatrix Lambda(n_draws, n_intervals);
  if(!is_cumulative) {
    Lambda = matrix_cumsum_columns(rate);
  } else {
    Lambda = rate;
  }

  NumericVector interval_duration = (range_t.column(1) - range_t.column(0)) / n_intervals;

  int n_max_events = R::qpois(1.0 - tol, max(Lambda), 1, 0);
  if(n_max_events == 0) {
    NumericMatrix Z(n_draws, 1); 
    std::fill( Z.begin(), Z.end(), NumericVector::get_na() ) ;
    return(Z);
  }

  NumericMatrix Z(n_draws, n_max_events); 
  std::fill( Z.begin(), Z.end(), NumericVector::get_na() ) ;


  NumericMatrix Tau(n_draws, n_max_events);
  for(int i =0; i!=n_draws*n_max_events; ++i) {
    Tau[i] = R::rexp(1);
  }
  if(n_max_events>1){
    matrix_cumsum_columns_inplace(Tau);  
  }
 

  return step_regular_inverse(Z, n_max_events, Lambda, Tau, interval_duration, range_t, atmost1);
}


