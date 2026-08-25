#include "sc_step_core.h"

using namespace Rcpp;

// Conditional (N >= atleastK) order-statistics sampler on an arbitrary grid.
// Whole-range sampling passes subinterval == the outer bounds. atmostK /
// budget_cap <= 0 mean "off"; atleastK must be >= 1 (checked at the R level).
// [[Rcpp::export]]
NumericMatrix vztdraw_sc_step_general2(
  const NumericMatrix & rate,
  const bool is_cumulative,
  const NumericMatrix & time_breaks,
  const NumericMatrix & subinterval,
  const double tol,
  const int atmostK,
  const int atleastK,
  const int budget_cap
) {
  const nhppp::GeneralGrid grid(time_breaks);
  return nhppp::sc_step_zt_core(rate, is_cumulative, grid, subinterval,
                                tol, atmostK, atleastK, budget_cap);
}
