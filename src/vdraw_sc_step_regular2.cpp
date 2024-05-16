#include "nhppp.h"

using namespace Rcpp; 

// [[Rcpp::export]]
NumericMatrix vdraw_sc_step_regular2(
  const NumericMatrix & rate,
  const bool is_cumulative,
  const NumericMatrix & range_t,
  const NumericMatrix & subinterval,
  const double tol,
  const bool atmost1, 
  const int atmostB
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

  int n_max_events = R::qpois(1.0 - tol, max(Lambda), 1, 0);
  if(atmostB>0 && atmostB < n_max_events) {
    n_max_events = atmostB;
  }
  
  if(n_max_events == 0) {
    NumericMatrix Z(n_draws, 1); 
    std::fill( Z.begin(), Z.end(), NumericVector::get_na() ) ;
    return(Z);
  }

  NumericMatrix Z(n_draws, n_max_events);
  std::fill( Z.begin(), Z.end(), NumericVector::get_na() ) ;
  int i0, i1, j0, ev;
  double f0, f1, L0, L1, tau, L_at_start_of_j0; 
  int ev_max = 0;
  for (int draw = 0; draw != n_draws; ++draw){

    auto L = Lambda.row(draw);
    
    // i0, i1, the indices of the intervals for the subinterval bounds
    // f0 (f1) the fraction of the interval i0 (i1) where the lower (upper) subinterval lies
    // L0, L1 , the cumulative intensity at the subinterval bounds 
    i0 = floor((subinterval(draw, 0) - range_t(draw, 0)) / interval_duration[draw]);
    f0 = (subinterval(draw, 0) - range_t(draw,0)) / interval_duration[draw] - i0; 
    L0 = (i0!=0)?L[i0-1]:0;
    L0 = simple_lerp(L0, L[i0], f0);
    i1 = floor((subinterval(draw, 1) - range_t(draw, 0)) / interval_duration[draw]);
    f1 = (subinterval(draw, 1) - range_t(draw,0)) / interval_duration[draw] - i1; 
    L1 = (i1!=0)?L[i1-1]:0;
    L1 = (i1 != n_intervals)?simple_lerp(L1,L[i1], f1):L[i1-1];

    tau = L0; 
    j0 = i0;
    ev = 0; 
    while(true){
      tau += R::rexp(1);
      if (tau > L1) {
        break; 
      }
      j0 = find_upper_bound_index(L, j0, tau); 
      if(j0 == -1) {
        break;
      }
      L_at_start_of_j0 = (j0>0) ? L[j0-1] : 0;
      Z(draw, ev) = range_t(draw, 0) + interval_duration[draw] * 
          (j0 + (tau - L_at_start_of_j0)/(L[j0] - L_at_start_of_j0));
      if(atmost1){
        break;
      }
      ev_max = std::max(ev_max, ev);
      ev++;
      if(ev == n_max_events) {
        break;
      }
    }
  }

  return Z(Rcpp::Range(0, n_draws-1), Rcpp::Range(0, ev_max));
}

