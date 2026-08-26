#include "sc_step_core.h"

using namespace Rcpp;

// Unconditional inversion sampler on an arbitrary grid (time_breaks: 1 or
// n_draws rows, K+1 columns). Whole-range sampling passes subinterval == the
// outer bounds. All option ints: <= 0 means "off".
// [[Rcpp::export]]
NumericMatrix vdraw_sc_step_general2(
  const NumericMatrix & rate,
  const bool is_cumulative,
  const NumericMatrix & time_breaks,
  const NumericMatrix & subinterval,
  const double tol,
  const int report_first_K,
  const int report_last_K,
  const int budget_cap
) {
  const nhppp::GeneralGrid grid(time_breaks);
  return nhppp::sc_step_core(rate, is_cumulative, grid, subinterval,
                             tol, report_first_K, report_last_K, budget_cap);
}
