#include "intensity_step_core.h"
using namespace Rcpp;

// Thinning sampler on a regular grid: candidates from the piecewise constant
// majorizer, accepted with probability lambda(t) / lambda_maj(interval of t).
// See intensity_step_core.h for the reporting / conditioning contract.
// [[Rcpp::export]]
NumericMatrix vdraw_intensity_step_regular(
  const Function & lambda,
  const NumericMatrix & rate_maj,
  const bool is_cumulative,
  const NumericMatrix & range_t,
  const NumericMatrix & subinterval,
  const bool use_subinterval,
  const double tol,
  const int report_first_K,
  const int report_last_K,
  const int gen_at_least_K,
  const int budget_cap) {
  const nhppp::RegularGrid grid(range_t, rate_maj.cols());
  return nhppp::intensity_step_core(
      lambda, rate_maj, is_cumulative, grid,
      use_subinterval ? subinterval : range_t,
      tol, report_first_K, report_last_K, gen_at_least_K, budget_cap);
}
