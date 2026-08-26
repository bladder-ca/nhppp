#include "intensity_step_core.h"
using namespace Rcpp;

// Thinning sampler on a regular grid: candidates from the piecewise constant
// majorizer via the inversion (or, when atleastK >= 1, the conditional
// order-statistics) kernel, accepted with probability
// lambda(t) / lambda_maj(interval of t). See intensity_step_core.h for the
// atleastK / atmostK contract.
// [[Rcpp::export]]
NumericMatrix vdraw_intensity_step_regular(
  const Function & lambda,
  const NumericMatrix & rate_maj,
  const bool is_cumulative,
  const NumericMatrix & range_t,
  const NumericMatrix & subinterval,
  const bool use_subinterval,
  const double tol,
  const int atmostK,
  const int atleastK,
  const int budget_cap) {
  const nhppp::RegularGrid grid(range_t, rate_maj.cols());
  return nhppp::intensity_step_core(
      lambda, rate_maj, is_cumulative, grid,
      use_subinterval ? subinterval : range_t,
      tol, atmostK, atleastK, budget_cap);
}
