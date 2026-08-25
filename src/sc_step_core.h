#ifndef NHPPP_SC_STEP_CORE_H
#define NHPPP_SC_STEP_CORE_H

#include "nhppp.h"

// Shared machinery for the piecewise-constant ("sc_step") samplers.
//
// Two grid policies describe the time axis (equal-length intervals vs
// arbitrary breaks); two template cores implement the exact algorithms of
// Trikalinos & Sereda (arXiv:2402.00358):
//   sc_step_core    - inversion of a unit-rate Exp(1) walk on the Lambda
//                     scale; events are generated in time order, so stopping
//                     after atmostK events yields exactly the earliest K.
//   sc_step_zt_core - conditional order statistics: N ~ Pois(L1-L0) given
//                     N >= atleastK, then N sorted uniforms on (L0, L1);
//                     under atmostK the K smallest of all N are reported
//                     (the count N itself is never truncated by atmostK).
// Both cores sample on a per-draw subinterval; whole-range sampling is the
// special case subinterval == full range.
//
// Grids and the subinterval matrix may have 1 row (shared across draws) or
// one row per draw. Cumulative Lambda columns hold values at interval ends
// (no leading zero). Zero-event rows are NA-filled; output is trimmed to the
// max event count (min 1 column).

namespace nhppp {

// std::upper_bound over a contiguous row buffer; -1 if tau exceeds all entries.
inline int upper_bound_index(const double* L, const int K, const int start, const double tau) {
  const double* it = std::upper_bound(L + start, L + K, tau);
  return (it == L + K) ? -1 : static_cast<int>(it - L);
}

// Equal-length intervals: range_t (1 or n_draws rows; columns t_min, t_max).
struct RegularGrid {
  const Rcpp::NumericMatrix& range_t;
  const int K;
  const bool shared;
  Rcpp::NumericVector dt_; // one interval duration per stored row

  RegularGrid(const Rcpp::NumericMatrix& range_t_, const int n_intervals)
    : range_t(range_t_), K(n_intervals), shared(range_t_.rows() == 1),
      dt_((range_t_.column(1) - range_t_.column(0)) / n_intervals) {}

  int row(const int d) const { return shared ? 0 : d; }
  double t0(const int d) const { return range_t(row(d), 0); }
  double dt(const int d) const { return dt_[row(d)]; }
  double width(const int d, const int) const { return dt(d); }

  // interval index of time t, clamped to [0, K-1]; f = fractional position
  // inside that interval, clamped to [0, 1] so that boundary values never
  // index or interpolate out of range.
  int locate(const int d, const double t, double& f) const {
    const double r = (t - t0(d)) / dt(d);
    int j = static_cast<int>(std::floor(r));
    if (j > K - 1) j = K - 1;
    if (j < 0) j = 0;
    f = r - j;
    if (f > 1.0) f = 1.0;
    if (f < 0.0) f = 0.0;
    return j;
  }

  double time_at(const int d, const int j, const double f) const {
    return t0(d) + dt(d) * (j + f);
  }
};

// Arbitrary interval bounds: time_breaks (1 or n_draws rows, K+1 columns).
struct GeneralGrid {
  const Rcpp::NumericMatrix& B;
  const int K;
  const bool shared;

  explicit GeneralGrid(const Rcpp::NumericMatrix& time_breaks)
    : B(time_breaks), K(time_breaks.cols() - 1), shared(time_breaks.rows() == 1) {}

  int row(const int d) const { return shared ? 0 : d; }
  double width(const int d, const int j) const {
    const int r = row(d);
    return B(r, j + 1) - B(r, j);
  }

  int locate(const int d, const double t, double& f) const {
    const int r = row(d);
    int lo = 0, hi = K + 1;
    while (lo < hi) { // smallest index with B(r, index) > t
      const int mid = (lo + hi) / 2;
      if (B(r, mid) > t) {
        hi = mid;
      } else {
        lo = mid + 1;
      }
    }
    int j = lo - 1;
    if (j > K - 1) j = K - 1;
    if (j < 0) j = 0;
    f = (t - B(r, j)) / (B(r, j + 1) - B(r, j));
    if (f > 1.0) f = 1.0;
    if (f < 0.0) f = 0.0;
    return j;
  }

  double time_at(const int d, const int j, const double f) const {
    const int r = row(d);
    return B(r, j) + (B(r, j + 1) - B(r, j)) * f;
  }
};

// Cumulative intensity at interval ends. When is_cumulative, the returned
// matrix ALIASES the caller's R object — the cores must never write to it.
template <class Grid>
Rcpp::NumericMatrix build_Lambda(const Rcpp::NumericMatrix& rate,
                                 const bool is_cumulative, const Grid& g) {
  if (is_cumulative) {
    return rate;
  }
  const int n_draws = rate.rows(), K = rate.cols();
  Rcpp::NumericMatrix Lambda(n_draws, K);
  for (int d = 0; d != n_draws; ++d) {
    double acc = 0.0;
    for (int j = 0; j != K; ++j) {
      acc += rate(d, j) * g.width(d, j);
      Lambda(d, j) = acc;
    }
  }
  return Lambda;
}

inline Rcpp::NumericMatrix na_matrix(const int n_draws, const int n_cols) {
  Rcpp::NumericMatrix Z(n_draws, n_cols);
  std::fill(Z.begin(), Z.end(), Rcpp::NumericVector::get_na());
  return Z;
}

inline Rcpp::NumericMatrix trim_columns(Rcpp::NumericMatrix& Z, const int ev_max,
                                        const int n_draws) {
  if (ev_max + 1 == Z.cols()) {
    return Z;
  }
  return Z(Rcpp::Range(0, n_draws - 1), Rcpp::Range(0, ev_max));
}

// Inversion sampler (Exp(1) walk). atmostK / budget_cap <= 0 mean "off";
// they act identically here because events arrive in time order.
template <class Grid>
Rcpp::NumericMatrix sc_step_core(const Rcpp::NumericMatrix& rate,
                                 const bool is_cumulative, const Grid& g,
                                 const Rcpp::NumericMatrix& subinterval,
                                 const double tol, const int atmostK,
                                 const int budget_cap) {
  const int K = rate.cols();
  const int n_draws = rate.rows();
  const bool shared_sub = (subinterval.rows() == 1);
  const Rcpp::NumericMatrix Lambda = build_Lambda(rate, is_cumulative, g);

  int n_max_events = safe_double_to_int(R::qpois(1.0 - tol, Rcpp::max(Lambda), 1, 0));
  if (budget_cap > 0 && budget_cap < n_max_events) n_max_events = budget_cap;
  if (atmostK > 0 && atmostK < n_max_events) n_max_events = atmostK;
  if (n_max_events == 0) {
    return na_matrix(n_draws, 1);
  }

  Rcpp::NumericMatrix Z = na_matrix(n_draws, n_max_events);
  std::vector<double> L(K); // contiguous copy of the current row of Lambda
  int ev_max = 0;
  double f0, f1;
  for (int d = 0; d != n_draws; ++d) {
    for (int j = 0; j != K; ++j) L[j] = Lambda(d, j);
    const int sr = shared_sub ? 0 : d;
    const int i0 = g.locate(d, subinterval(sr, 0), f0);
    const double L0 = simple_lerp((i0 != 0) ? L[i0 - 1] : 0.0, L[i0], f0);
    const int i1 = g.locate(d, subinterval(sr, 1), f1);
    const double L1 = simple_lerp((i1 != 0) ? L[i1 - 1] : 0.0, L[i1], f1);

    double tau = L0;
    int j0 = i0;
    int ev = 0;
    while (true) {
      tau += R::rexp(1);
      if (tau > L1) break;
      j0 = upper_bound_index(L.data(), K, j0, tau);
      if (j0 == -1) break;
      const double L_prev = (j0 > 0) ? L[j0 - 1] : 0.0;
      Z(d, ev) = g.time_at(d, j0, (tau - L_prev) / (L[j0] - L_prev));
      ev_max = std::max(ev_max, ev);
      ++ev;
      if (ev == n_max_events) break;
    }
  }
  return trim_columns(Z, ev_max, n_draws);
}

// Conditional order-statistics sampler for N >= atleastK (atleastK >= 1).
// budget_cap (and the 1 - tol quantile) cap the count N, but never below
// atleastK; atmostK caps only how many of the N order statistics are
// reported (the K smallest), not N itself.
template <class Grid>
Rcpp::NumericMatrix sc_step_zt_core(const Rcpp::NumericMatrix& rate,
                                    const bool is_cumulative, const Grid& g,
                                    const Rcpp::NumericMatrix& subinterval,
                                    const double tol, const int atmostK,
                                    const int atleastK, const int budget_cap) {
  const int K = rate.cols();
  const int n_draws = rate.rows();
  const bool shared_sub = (subinterval.rows() == 1);
  const Rcpp::NumericMatrix Lambda = build_Lambda(rate, is_cumulative, g);

  int n_count_cap = safe_double_to_int(R::qpois(1.0 - tol, Rcpp::max(Lambda), 1, 0));
  if (budget_cap > 0 && budget_cap < n_count_cap) n_count_cap = budget_cap;
  if (n_count_cap < atleastK) n_count_cap = atleastK;
  if (n_count_cap < 1) n_count_cap = 1;
  const int n_cols = (atmostK > 0 && atmostK < n_count_cap) ? atmostK : n_count_cap;

  Rcpp::NumericMatrix Z = na_matrix(n_draws, n_cols);
  std::vector<double> L(K);
  std::vector<double> U(n_count_cap);
  int ev_max = 0;
  double f0, f1;
  for (int d = 0; d != n_draws; ++d) {
    for (int j = 0; j != K; ++j) L[j] = Lambda(d, j);
    const int sr = shared_sub ? 0 : d;
    const int i0 = g.locate(d, subinterval(sr, 0), f0);
    const double L0 = simple_lerp((i0 != 0) ? L[i0 - 1] : 0.0, L[i0], f0);
    const int i1 = g.locate(d, subinterval(sr, 1), f1);
    const double L1 = simple_lerp((i1 != 0) ? L[i1 - 1] : 0.0, L[i1], f1);

    int N = rbtpois(L1 - L0, atleastK);
    if (N > n_count_cap) N = n_count_cap;
    if (N == 0) { // only when L1 == L0 (measure-zero subinterval)
      continue;
    }
    for (int i = 0; i != N; ++i) {
      U[i] = R::runif(L0, L1);
    }
    const int n_report = (atmostK > 0 && atmostK < N) ? atmostK : N;
    if (n_report < N) {
      std::partial_sort(U.begin(), U.begin() + n_report, U.begin() + N);
    } else {
      std::sort(U.begin(), U.begin() + N);
    }

    int j0 = i0;
    for (int i = 0; i != n_report; ++i) {
      j0 = upper_bound_index(L.data(), K, j0, U[i]);
      if (j0 == -1) break;
      const double L_prev = (j0 > 0) ? L[j0 - 1] : 0.0;
      Z(d, i) = g.time_at(d, j0, (U[i] - L_prev) / (L[j0] - L_prev));
      ev_max = std::max(ev_max, i);
    }
  }
  return trim_columns(Z, ev_max, n_draws);
}

} // namespace nhppp

#endif
