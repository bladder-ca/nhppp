#include "nhppp.h"
using namespace Rcpp;


// Generalization of vdraw_sc_step_regular to arbitrary interval bounds:
// time_breaks (1 or n_draws rows, n_intervals+1 columns) replaces range_t.
// [[Rcpp::export]]
NumericMatrix vdraw_sc_step_general(
  const NumericMatrix & rate,
  const bool is_cumulative,
  const NumericMatrix & time_breaks,
  const double tol,
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

  return step_general_inverse(n_max_events, Lambda, Tau, time_breaks, atmost1);
}
