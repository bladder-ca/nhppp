#include "sc_step_core.h"

using namespace Rcpp;

// Conditioned (gen_at_least_K <= N <= gen_at_most_K) order-statistics
// sampler on an arbitrary grid. Whole-range sampling passes
// subinterval == the outer bounds. All option ints: <= 0 means "off"; at
// least one generation bound must be active (checked at the R level).
// [[Rcpp::export]]
NumericMatrix vztdraw_sc_step_general2(
  const NumericMatrix & rate,
  const bool is_cumulative,
  const NumericMatrix & time_breaks,
  const NumericMatrix & subinterval,
  const double tol,
  const int report_first_K,
  const int report_last_K,
  const int gen_at_least_K,
  const int gen_at_most_K,
  const int budget_cap
) {
  const nhppp::GeneralGrid grid(time_breaks);
  return nhppp::sc_step_orderstat_core(rate, is_cumulative, grid, subinterval,
                                       tol, report_first_K, report_last_K,
                                       gen_at_least_K, gen_at_most_K, budget_cap);
}
