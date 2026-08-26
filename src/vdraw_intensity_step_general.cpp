#include "intensity_step_core.h"
using namespace Rcpp;

// Thinning sampler on an arbitrary grid (time_breaks: 1 or n_draws rows,
// K+1 columns): candidates from the piecewise constant majorizer via the
// inversion (or, when atleastK >= 1, the conditional order-statistics)
// kernel, accepted with probability lambda(t) / lambda_maj(interval of t).
// See intensity_step_core.h for the atleastK / atmostK contract.
// [[Rcpp::export]]
NumericMatrix vdraw_intensity_step_general(
  const Function & lambda,
  const NumericMatrix & rate_maj,
  const bool is_cumulative,
  const NumericMatrix & time_breaks,
  const NumericMatrix & subinterval,
  const bool use_subinterval,
  const double tol,
  const int atmostK,
  const int atleastK,
  const int budget_cap) {
  const nhppp::GeneralGrid grid(time_breaks);
  NumericMatrix whole_range(time_breaks.rows(), 2);
  if (!use_subinterval) {
    whole_range(_, 0) = time_breaks(_, 0);
    whole_range(_, 1) = time_breaks(_, time_breaks.cols() - 1);
  }
  return nhppp::intensity_step_core(
      lambda, rate_maj, is_cumulative, grid,
      use_subinterval ? subinterval : whole_range,
      tol, atmostK, atleastK, budget_cap);
}
