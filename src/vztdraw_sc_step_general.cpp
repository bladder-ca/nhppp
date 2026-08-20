#include "nhppp.h"
using namespace Rcpp;


// Generalization of vztdraw_sc_step_regular to arbitrary interval bounds:
// time_breaks (1 or n_draws rows, n_intervals+1 columns) replaces range_t.
// [[Rcpp::export]]
NumericMatrix vztdraw_sc_step_general(
  const NumericMatrix & rate,
  const bool is_cumulative,
  const NumericMatrix & time_breaks,
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


  IntegerVector n_events = rztpois_vec(Lambda(_,n_intervals-1));
  int max_events = * std::max_element(n_events.begin(), n_events.end());
  int max_cols = (atmost1) ? 1 : max_events;

  NumericMatrix Tau(n_draws, max_cols);
  std::fill( Tau.begin(), Tau.end(), NumericVector::get_na() ) ;

  NumericVector tmp(max_events);

  for(int r = 0; r!= n_draws; ++r) {
    for(int ev = 0; ev != n_events[r]; ++ev){
      tmp[ev] = R::runif(0, 1);
    }
    if(atmost1){
      Tau(r,0) = (*std::min_element(tmp.begin(), tmp.begin()+n_events[r])) *
                  Lambda(r, n_intervals-1);
    } else {
      std::sort(tmp.begin(), tmp.begin()+n_events[r]);
      for(int ev = 0; ev != n_events[r]; ++ev){
        Tau(r, ev) = tmp[ev] * Lambda(r, n_intervals-1);
      }
    }
  }

  return step_general_inverse(max_events, Lambda, Tau, time_breaks, atmost1);
}
