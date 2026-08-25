#include <string>
#include "sc_step_core.h"
using namespace Rcpp;

// Thinning sampler on a regular grid: draw candidates from the piecewise
// constant majorizer via the inversion kernel, then accept each candidate
// with probability lambda(t) / lambda_maj(interval of t).
// [[Rcpp::export]]
NumericMatrix vdraw_intensity_step_regular(
  const Function & lambda,
  const NumericMatrix & rate_maj,
  const bool is_cumulative,
  const NumericMatrix & range_t,
  const NumericMatrix & subinterval,
  const bool use_subinterval,
  const double tol,
  const bool atmost1,
  const int budget_cap) {

  const int n_intervals = rate_maj.cols();
  const int n_draws = rate_maj.rows();
  const nhppp::RegularGrid grid(range_t, n_intervals);
  const double epsilon = std::numeric_limits<double>::epsilon();

  // Lambda_maj: cumulative majorizer at interval ends; lambda_maj: per-interval
  // rates. One of the two aliases rate_maj (read-only), the other is built.
  NumericMatrix Lambda_maj = nhppp::build_Lambda(rate_maj, is_cumulative, grid);
  NumericMatrix lambda_maj;
  if(!is_cumulative) {
    lambda_maj = rate_maj;
  } else {
    lambda_maj = NumericMatrix(n_draws, n_intervals);
    for(int draw = 0; draw != n_draws; ++draw){
      double prev = 0.0;
      for(int j = 0; j != n_intervals; ++j){
        lambda_maj(draw, j) = (rate_maj(draw, j) - prev) / grid.dt(draw);
        prev = rate_maj(draw, j);
      }
    }
  }

  // candidates: all majorizer events (atmostK off — thinning must see them all)
  NumericMatrix Zstar = nhppp::sc_step_core(
      Lambda_maj, true, grid, use_subinterval ? subinterval : range_t,
      tol, 0, budget_cap);

  double acceptance_prob, f;
  int interval;
  int acc_i = 0;
  int max_acc_i = 0;

  NumericMatrix lambda_star = lambda(Zstar);

  NumericMatrix Z(n_draws, Zstar.cols());
  std::fill(Z.begin(), Z.end(), NumericVector::get_na());

  for(int draw = 0; draw != n_draws; ++draw){
    acc_i = 0;
    for(int ev = 0; ev != Zstar.cols(); ++ev){
      if(NumericVector::is_na(Zstar(draw, ev))) {
        break;
      }
      interval = grid.locate(draw, Zstar(draw, ev), f);
      acceptance_prob = (lambda_star(draw, ev)/lambda_maj(draw, interval));
      if(acceptance_prob > 1.0 + 5*epsilon || acceptance_prob < 0.0 - 5*epsilon) {
        std::string str = "Majorizer error? Pr(acceptance) = ";
        str += std::to_string(acceptance_prob);
        throw std::range_error(str);
      }

      if(acceptance_prob > (R::runif(0.0, 1.0))) {
        Z(draw,acc_i) = Zstar(draw, ev);
        max_acc_i = std::max(max_acc_i, acc_i);
        ++acc_i;
        if(atmost1) {
          break;
        }
      }
    }
  }

  return Z(Range(0, n_draws-1), Range(0, max_acc_i));
}
