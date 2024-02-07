#include "nhppp.h"


// [[Rcpp::export]]
Rcpp::NumericMatrix step_regular_inverse(
  Rcpp::NumericMatrix & Z, 
  int max_events,
  const Rcpp::NumericMatrix & Lambda,
  const Rcpp::NumericMatrix & Tau, 
  const Rcpp::NumericVector & interval_duration,
  const Rcpp::NumericVector & range_t, 
  bool atmost1
) {

  Rcpp::NumericVector L;
  int i1, i2, ev_max = 0;
  int n_draws = Lambda.rows(); 
  int n_intervals = Lambda.cols();
  double L0;

  for(int draw = 0; draw != n_draws; ++draw){
    i1 = 0; 
    i2 = 0;
    L = Lambda.row(draw);
    for(int ev = 0; ev != max_events; ++ev){
      if(Tau(draw, ev) > L[n_intervals-1]) {
        break;
      }
      i2 = find_upper_bound_index(L, i1, Tau(draw, ev));
      if(i2 == -1) {
        break;
      }
      L0 = (i2>0) ? L[i2-1] : 0;

      Z(draw, ev) = range_t(draw, 0) + 
        interval_duration[draw] * (
          i2 +
          (Tau(draw, ev) - L0) / (L[i2] - L0)
        );

      if(atmost1){
        break;
      }
      i1 = i2;
      ev_max = std::max(ev_max, ev);
    }
  }
  return Z(Rcpp::Range(0, n_draws-1), Rcpp::Range(0, ev_max));
}