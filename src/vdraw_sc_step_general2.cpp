#include "nhppp.h"

using namespace Rcpp;

// Generalization of vdraw_sc_step_regular2 to arbitrary interval bounds:
// time_breaks (1 or n_draws rows, n_intervals+1 columns) replaces range_t.
// [[Rcpp::export]]
NumericMatrix vdraw_sc_step_general2(
  const NumericMatrix & rate,
  const bool is_cumulative,
  const NumericMatrix & time_breaks,
  const NumericMatrix & subinterval,
  const double tol,
  const bool atmost1,
  const int atmostB
) {
  int n_intervals = rate.cols();
  int n_draws = rate.rows();
  const bool shared_breaks = (time_breaks.rows() == 1);
  NumericMatrix Lambda(n_draws, n_intervals);
  if(!is_cumulative) {
    for(int draw = 0; draw != n_draws; ++draw){
      const int br = shared_breaks ? 0 : draw;
      double acc = 0.0;
      for(int j = 0; j != n_intervals; ++j){
        acc += rate(draw, j) * (time_breaks(br, j + 1) - time_breaks(br, j));
        Lambda(draw, j) = acc;
      }
    }
  } else {
    Lambda = rate;
  }

  int n_max_events = safe_double_to_int(R::qpois(1.0 - tol, max(Lambda), 1, 0));
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

    const int br = shared_breaks ? 0 : draw;
    auto L = Lambda.row(draw);

    // i0, i1, the indices of the intervals for the subinterval bounds
    // f0 (f1) the fraction of the interval i0 (i1) where the lower (upper) subinterval lies
    // L0, L1 , the cumulative intensity at the subinterval bounds
    i0 = find_break_interval(time_breaks, br, subinterval(draw, 0), f0);
    L0 = (i0!=0)?L[i0-1]:0;
    L0 = simple_lerp(L0, L[i0], f0);
    i1 = find_break_interval(time_breaks, br, subinterval(draw, 1), f1);
    L1 = (i1!=0)?L[i1-1]:0;
    L1 = simple_lerp(L1, L[i1], f1);

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
      Z(draw, ev) = time_breaks(br, j0) + (time_breaks(br, j0 + 1) - time_breaks(br, j0)) *
          (tau - L_at_start_of_j0)/(L[j0] - L_at_start_of_j0);
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
