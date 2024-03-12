#include "nhppp.h"

using namespace Rcpp; 

// [[Rcpp::export]]
NumericMatrix vztdraw_sc_step_regular2(
  const NumericMatrix & rate,
  const bool is_cumulative,
  const NumericMatrix & range_t,
  const NumericMatrix & subinterval,
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

  int n_max_events = std::max(1, (int) R::qpois(.99999, max(Lambda), 1, 0));
  NumericVector U(n_max_events);
  NumericMatrix Z(n_draws, n_max_events);
  std::fill( Z.begin(), Z.end(), NumericVector::get_na() ) ;
  int i0, i1, j0, N, N_iter;
  double f0, f1, L0, L1, L_at_start_of_j0, u; 
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


    N = rztpois(L1 - L0);
    for (int i = 0; i != N; ++i){
      U[i] = R::runif(L0, L1);
    }
    if(atmost1){
      u = (*std::min_element(U.begin(), U.begin() + N));
      U[0] = u;
      N_iter = 1;
    } else {
      std::sort(U.begin(), U.begin() + N); 
      N_iter = N;
    }
        
    j0 = i0;
    for(int i = 0; i != N_iter; ++i){
      j0 = find_upper_bound_index(L, i0, U[i]); 
      if(j0 == -1) {
        break;
      }
      L_at_start_of_j0 = (j0>0) ? L[j0-1] : 0;
      Z(draw, i) =  range_t(draw, 0) + interval_duration[draw] * 
          (j0 + (U[i] - L_at_start_of_j0)/(L[j0] - L_at_start_of_j0));
      if(atmost1){
        break;
      }
      ev_max = std::max(ev_max, i);
    }
  }
  return Z(Rcpp::Range(0, n_draws-1), Rcpp::Range(0, ev_max));
}

