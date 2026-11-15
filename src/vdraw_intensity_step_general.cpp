#include "intensity_step_core.h"
using namespace Rcpp;

// Thinning sampler on an arbitrary grid (time_breaks: 1 or n_draws rows,
// K+1 columns): candidates from the piecewise constant majorizer, accepted
// with probability lambda(t) / lambda_maj(interval of t).
// See intensity_step_core.h for the reporting / conditioning contract.
// [[Rcpp::export]]
NumericMatrix vdraw_intensity_step_general(
  const Function & lambda,
  const NumericMatrix & rate_maj,
  const bool is_cumulative,
  const NumericMatrix & time_breaks,
  const NumericMatrix & subinterval,
  const bool use_subinterval,
  const double tol,
  const int report_first_K,
  const int report_last_K,
  const int gen_at_least_K,
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
      tol, report_first_K, report_last_K, gen_at_least_K, budget_cap);
}
