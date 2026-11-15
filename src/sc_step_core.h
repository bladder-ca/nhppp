#ifndef NHPPP_SC_STEP_CORE_H
#define NHPPP_SC_STEP_CORE_H

#include "nhppp.h"

// Shared machinery for the piecewise-constant ("sc_step") samplers.
//
// Two grid policies describe the time axis (equal-length intervals vs
// arbitrary breaks); two template cores implement the exact algorithms of
// Trikalinos & Sereda (arXiv:2402.00358):
//   sc_step_core      - inversion of a unit-rate Exp(1) walk on the Lambda
//                       scale; the UNCONDITIONAL sampler.
//   sc_step_orderstat_core - conditional order statistics:
//                       N ~ Pois(L1-L0) truncated to
//                       [gen_at_least_K, gen_at_most_K], then N sorted
//                       uniforms on (L0, L1); the CONDITIONED sampler.
// Both cores sample on a per-draw subinterval; whole-range sampling is the
// special case subinterval == full range.
//
// Option classes (all ints, <= 0 = off):
//   report_first_K / report_last_K - REPORTING truncations: return only the
//     earliest / latest min(N, K) events of the realization; the count law
//     is unchanged. At most one may be set (enforced at the R level). The
//     sequential core generates in time order, so report_first_K may stop
//     generation early; report_last_K requires the full realization first.
//   gen_at_least_K / gen_at_most_K - GENERATION conditioning: change the
//     sampled law to X | K1 <= N <= K2 (orderstat core only; the sequential
//     walk cannot condition on the future total).
//   budget_cap - computational cap on the event budget (approximation knob,
//     together with the 1 - tol quantile bound); never truncates the count
//     below gen_at_least_K.
//
// Grids and the subinterval matrix may have 1 row (shared across draws) or
// one row per draw. Cumulative Lambda columns hold values at interval ends
// (no leading zero). Empty rows are NA-filled; output is trimmed to the max
// reported event count (min 1 column).
//
// Output sinks: the cores are additionally templated on an output policy.
//   DenseSink - the NA-padded event matrix (one row per draw); the default,
//     so existing call sites compile unchanged.
//   LongSink  - long format: parallel vectors (id, time), one entry per
//     event, ids 1-based and ascending, times ascending within id, plus
//     n_draws. A draw with zero events contributes no entries; NA never
//     appears. Events are accumulated in std::vectors (reserve()d from the
//     expected Lambda mass, geometric growth beyond that) and copied once
//     into R vectors at the end.
// The sink only records events that the algorithms emit -- RNG consumption
// is identical across sinks, so a same-seed dense and long run hold the
// same event values.

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

// Keep only the latest n_keep of the n_generated (ascending) events in row d.
inline int compact_last(Rcpp::NumericMatrix& Z, const int d,
                        const int n_generated, const int n_keep) {
  if (n_keep <= 0 || n_generated <= n_keep) {
    return n_generated;
  }
  for (int i = 0; i != n_keep; ++i) {
    Z(d, i) = Z(d, n_generated - n_keep + i);
  }
  for (int i = n_keep; i != n_generated; ++i) {
    Z(d, i) = Rcpp::NumericVector::get_na();
  }
  return n_keep;
}

// The NA-padded event-matrix output (the historical behavior).
struct DenseSink {
  static constexpr bool is_long = false;
  typedef Rcpp::NumericMatrix result_type;
  Rcpp::NumericMatrix Z;
  const int n_draws;
  int ev_max;

  DenseSink(const int n_draws_, const int n_cols, const double)
    : Z(na_matrix(n_draws_, n_cols)), n_draws(n_draws_), ev_max(0) {}

  void emit(const int d, const int ev, const double t) { Z(d, ev) = t; }

  void end_row(const int d, const int n_generated, const int report_last_K) {
    const int ev = compact_last(Z, d, n_generated, report_last_K);
    if (ev > 0) ev_max = std::max(ev_max, ev - 1);
  }

  result_type result() { return trim_columns(Z, ev_max, n_draws); }
};

// Long-format output: list(id, time, n_draws). Zero-event draws contribute
// no entries (their id is absent); no NA anywhere. expected_events is a
// reserve() hint only -- correctness does not depend on it.
struct LongSink {
  static constexpr bool is_long = true;
  typedef Rcpp::List result_type;
  std::vector<int> id;
  std::vector<double> time;
  const int n_draws;

  LongSink(const int n_draws_, const int, const double expected_events)
    : n_draws(n_draws_) {
    if (expected_events > 0) {
      const std::size_t r = static_cast<std::size_t>(expected_events);
      id.reserve(r);
      time.reserve(r);
    }
  }

  void emit(const int d, const int, const double t) {
    id.push_back(d + 1);
    time.push_back(t);
  }

  // report_last_K analog of compact_last: the current draw's events are the
  // vector tail, so keep the last n_keep of that segment and shrink.
  void end_row(const int, const int n_generated, const int report_last_K) {
    if (report_last_K <= 0 || n_generated <= report_last_K) return;
    const std::size_t start = time.size() - n_generated;
    const std::size_t from = time.size() - report_last_K;
    for (int i = 0; i != report_last_K; ++i) {
      time[start + i] = time[from + i];
    }
    time.resize(start + report_last_K);
    id.resize(start + report_last_K);
  }

  result_type result() {
    return Rcpp::List::create(
      Rcpp::Named("id") = Rcpp::IntegerVector(id.begin(), id.end()),
      Rcpp::Named("time") = Rcpp::NumericVector(time.begin(), time.end()),
      Rcpp::Named("n_draws") = n_draws);
  }
};

// reserve() hint for LongSink: expected whole-range event mass sum(Lambda)
// plus a 4-sigma Poisson margin, floored at n_draws * k_min events under a
// lower generation bound and capped at the per-draw event cap. The
// whole-range mass upper-bounds any subinterval mass.
inline double long_reserve_hint(const Rcpp::NumericMatrix& Lambda,
                                const int k_min, const int per_draw_cap) {
  const int K = Lambda.cols();
  const int n_draws = Lambda.rows();
  double mass = 0.0;
  for (int d = 0; d != n_draws; ++d) mass += Lambda(d, K - 1);
  if (k_min > 0) mass = std::max(mass, static_cast<double>(n_draws) * k_min);
  mass += 4.0 * std::sqrt(mass) + 16.0;
  return std::min(mass, static_cast<double>(n_draws) * per_draw_cap);
}

// Unconditional inversion sampler (Exp(1) walk). Events arrive in time
// order, so report_first_K may cut generation short; report_last_K must let
// the walk run to the end of the (sub)interval and compacts afterwards.
template <class Grid, class Sink = DenseSink>
typename Sink::result_type sc_step_core(const Rcpp::NumericMatrix& rate,
                                        const bool is_cumulative, const Grid& g,
                                        const Rcpp::NumericMatrix& subinterval,
                                        const double tol, const int report_first_K,
                                        const int report_last_K, const int budget_cap) {
  const int K = rate.cols();
  const int n_draws = rate.rows();
  const bool shared_sub = (subinterval.rows() == 1);
  const Rcpp::NumericMatrix Lambda = build_Lambda(rate, is_cumulative, g);

  int n_max_events = safe_double_to_int(R::qpois(1.0 - tol, Rcpp::max(Lambda), 1, 0));
  if (budget_cap > 0 && budget_cap < n_max_events) n_max_events = budget_cap;
  if (report_first_K > 0 && report_first_K < n_max_events) n_max_events = report_first_K;
  // report_last_K cannot shrink the generation budget: the whole realization
  // must be generated before the last K are known
  const double hint =
    (Sink::is_long && n_max_events > 0) ? long_reserve_hint(Lambda, 0, n_max_events) : 0.0;
  Sink sink(n_draws, std::max(n_max_events, 1), hint);
  if (n_max_events == 0) {
    return sink.result();
  }

  std::vector<double> L(K); // contiguous copy of the current row of Lambda
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
      sink.emit(d, ev, g.time_at(d, j0, (tau - L_prev) / (L[j0] - L_prev)));
      ++ev;
      if (ev == n_max_events) break;
    }
    sink.end_row(d, ev, report_last_K);
  }
  return sink.result();
}

// Conditional order-statistics sampler for gen_at_least_K <= N <=
// gen_at_most_K (either bound may be off). budget_cap (and the 1 - tol
// quantile) cap the count N, but never below gen_at_least_K; the reporting
// options select which min(N, K) order statistics are returned and never
// affect N itself.
template <class Grid, class Sink = DenseSink>
typename Sink::result_type sc_step_orderstat_core(const Rcpp::NumericMatrix& rate,
                                                  const bool is_cumulative, const Grid& g,
                                                  const Rcpp::NumericMatrix& subinterval,
                                                  const double tol,
                                                  const int report_first_K,
                                                  const int report_last_K,
                                                  const int gen_at_least_K,
                                                  const int gen_at_most_K,
                                                  const int budget_cap) {
  const int K = rate.cols();
  const int n_draws = rate.rows();
  const bool shared_sub = (subinterval.rows() == 1);
  const int k_min = (gen_at_least_K > 0) ? gen_at_least_K : 0;
  const Rcpp::NumericMatrix Lambda = build_Lambda(rate, is_cumulative, g);

  int n_count_cap = safe_double_to_int(R::qpois(1.0 - tol, Rcpp::max(Lambda), 1, 0));
  if (budget_cap > 0 && budget_cap < n_count_cap) n_count_cap = budget_cap;
  if (gen_at_most_K > 0 && gen_at_most_K < n_count_cap) n_count_cap = gen_at_most_K;
  if (n_count_cap < k_min) n_count_cap = k_min;
  if (n_count_cap < 1) n_count_cap = 1;
  int n_cols = n_count_cap;
  if (report_first_K > 0 && report_first_K < n_cols) n_cols = report_first_K;
  if (report_last_K > 0 && report_last_K < n_cols) n_cols = report_last_K;

  const int per_draw_reported = n_cols;
  const double hint =
    Sink::is_long ? long_reserve_hint(Lambda, k_min, per_draw_reported) : 0.0;
  Sink sink(n_draws, n_cols, hint);
  std::vector<double> L(K);
  std::vector<double> U(n_count_cap);
  double f0, f1;
  for (int d = 0; d != n_draws; ++d) {
    for (int j = 0; j != K; ++j) L[j] = Lambda(d, j);
    const int sr = shared_sub ? 0 : d;
    const int i0 = g.locate(d, subinterval(sr, 0), f0);
    const double L0 = simple_lerp((i0 != 0) ? L[i0 - 1] : 0.0, L[i0], f0);
    const int i1 = g.locate(d, subinterval(sr, 1), f1);
    const double L1 = simple_lerp((i1 != 0) ? L[i1 - 1] : 0.0, L[i1], f1);

    int N = rbtpois(L1 - L0, k_min, gen_at_most_K);
    if (N > n_count_cap) N = n_count_cap;
    if (N == 0) { // legitimate under a pure upper bound; else measure-zero subinterval
      continue;
    }
    for (int i = 0; i != N; ++i) {
      U[i] = R::runif(L0, L1);
    }
    int n_report = N;
    int offset = 0;
    if (report_first_K > 0 && report_first_K < N) {
      n_report = report_first_K;
    } else if (report_last_K > 0 && report_last_K < N) {
      n_report = report_last_K;
      offset = N - report_last_K;
    }
    if (offset == 0 && n_report < N) {
      std::partial_sort(U.begin(), U.begin() + n_report, U.begin() + N);
    } else {
      std::sort(U.begin(), U.begin() + N);
    }

    int j0 = i0;
    int emitted = 0;
    for (int i = 0; i != n_report; ++i) {
      j0 = upper_bound_index(L.data(), K, j0, U[offset + i]);
      if (j0 == -1) break;
      const double L_prev = (j0 > 0) ? L[j0 - 1] : 0.0;
      sink.emit(d, i, g.time_at(d, j0, (U[offset + i] - L_prev) / (L[j0] - L_prev)));
      ++emitted;
    }
    sink.end_row(d, emitted, 0); // reporting was applied pre-emission
  }
  return sink.result();
}

} // namespace nhppp

#endif
