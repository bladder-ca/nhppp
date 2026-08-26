#include "sc_step_core.h"

using namespace Rcpp;

// Unconditional inversion sampler on a regular grid (equal-length intervals
// given by range_t, 1 or n_draws rows). Whole-range sampling passes
// subinterval == range_t. All option ints: <= 0 means "off".
// [[Rcpp::export]]
NumericMatrix vdraw_sc_step_regular2(
  const NumericMatrix & rate,
  const bool is_cumulative,
  const NumericMatrix & range_t,
  const NumericMatrix & subinterval,
  const double tol,
  const int report_first_K,
  const int report_last_K,
  const int budget_cap
) {
  const nhppp::RegularGrid grid(range_t, rate.cols());
  return nhppp::sc_step_core(rate, is_cumulative, grid, subinterval,
                             tol, report_first_K, report_last_K, budget_cap);
}
