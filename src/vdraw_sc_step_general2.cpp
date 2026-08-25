#include "sc_step_core.h"

using namespace Rcpp;

// Inversion sampler on an arbitrary grid (time_breaks: 1 or n_draws rows,
// K+1 columns). Whole-range sampling passes subinterval == the outer bounds.
// atmostK / budget_cap <= 0 mean "off".
// [[Rcpp::export]]
NumericMatrix vdraw_sc_step_general2(
  const NumericMatrix & rate,
  const bool is_cumulative,
  const NumericMatrix & time_breaks,
  const NumericMatrix & subinterval,
  const double tol,
  const int atmostK,
  const int budget_cap
) {
  const nhppp::GeneralGrid grid(time_breaks);
  return nhppp::sc_step_core(rate, is_cumulative, grid, subinterval,
                             tol, atmostK, budget_cap);
}
