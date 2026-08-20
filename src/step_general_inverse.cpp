#include "nhppp.h"

// Locate the interval of a (possibly irregular) time grid that contains t.
// B has K+1 break points per row (1 row = shared across draws, else 1 per draw).
// Returns j in [0, K-1] with B(row, j) <= t < B(row, j+1), clamped at the
// boundaries; f is the fractional position of t inside interval j, clamped
// to [0, 1] so that boundary values never index or interpolate out of range.
int find_break_interval(
  const Rcpp::NumericMatrix & B,
  const int row,
  const double t,
  double & f
) {
  const int K = B.cols() - 1;
  int lo = 0, hi = K + 1;
  while (lo < hi) {  // smallest index with B(row, index) > t
    const int mid = (lo + hi) / 2;
    if (B(row, mid) > t) {
      hi = mid;
    } else {
      lo = mid + 1;
    }
  }
  int j = lo - 1;
  if (j > K - 1) j = K - 1;
  if (j < 0) j = 0;
  f = (t - B(row, j)) / (B(row, j + 1) - B(row, j));
  if (f > 1.0) f = 1.0;
  if (f < 0.0) f = 0.0;
  return j;
}


// Generalization of step_regular_inverse to arbitrary interval bounds:
// time_breaks (1 or n_draws rows, n_intervals+1 columns) replaces range_t.
// [[Rcpp::export]]
Rcpp::NumericMatrix step_general_inverse(
  const int max_events,
  const Rcpp::NumericMatrix & Lambda,
  const Rcpp::NumericMatrix & Tau,
  const Rcpp::NumericMatrix & time_breaks,
  const bool atmost1
) {

  int i1, i2, ev_max = 0;
  int n_draws = Lambda.rows();
  int n_intervals = Lambda.cols();
  double L0;
  const bool shared_breaks = (time_breaks.rows() == 1);

  Rcpp::NumericMatrix Z(n_draws, max_events);
  std::fill( Z.begin(), Z.end(), Rcpp::NumericVector::get_na() ) ;

  for(int draw = 0; draw != n_draws; ++draw){
    const int br = shared_breaks ? 0 : draw;
    i1 = 0;
    i2 = 0;
    auto L = Lambda.row(draw);
    for(int ev = 0; ev != max_events; ++ev){
      if(Tau(draw, ev) > L[n_intervals-1]) {
        break;
      }
      i2 = find_upper_bound_index(L, i1, Tau(draw, ev));
      if(i2 == -1) {
        break;
      }
      L0 = (i2>0) ? L[i2-1] : 0;

      Z(draw, ev) = time_breaks(br, i2) +
        (time_breaks(br, i2 + 1) - time_breaks(br, i2)) *
          (Tau(draw, ev) - L0) / (L[i2] - L0);

      if(atmost1){
        break;
      }
      i1 = i2;
      ev_max = std::max(ev_max, ev);
    }
  }
  return Z(Rcpp::Range(0, n_draws-1), Rcpp::Range(0, ev_max));
}
