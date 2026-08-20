#include "nhppp.h"

using namespace Rcpp;

// Generalization of vztdraw_sc_step_regular2 to arbitrary interval bounds:
// time_breaks (1 or n_draws rows, n_intervals+1 columns) replaces range_t.
// [[Rcpp::export]]
NumericMatrix vztdraw_sc_step_general2(
  const NumericMatrix & rate,
  const bool is_cumulative,
  const NumericMatrix & time_breaks,
  const NumericMatrix & subinterval,
  const bool atmost1
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

  int n_max_events = std::max(1, safe_double_to_int(R::qpois(.99999, max(Lambda), 1, 0)));
  NumericVector U(n_max_events);
  NumericMatrix Z(n_draws, n_max_events);
  std::fill( Z.begin(), Z.end(), NumericVector::get_na() ) ;
  int i0, i1, j0, N, N_iter;
  double f0, f1, L0, L1, L_at_start_of_j0, u;
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


    N = rztpois(L1 - L0);
    N = std::min(N, n_max_events);
    if(N == 0) {  // only when L1 == L0 (measure-zero subinterval)
      continue;
    }
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
      Z(draw, i) = time_breaks(br, j0) + (time_breaks(br, j0 + 1) - time_breaks(br, j0)) *
          (U[i] - L_at_start_of_j0)/(L[j0] - L_at_start_of_j0);
      if(atmost1){
        break;
      }
      ev_max = std::max(ev_max, i);
    }
  }
  return Z(Rcpp::Range(0, n_draws-1), Rcpp::Range(0, ev_max));
}
