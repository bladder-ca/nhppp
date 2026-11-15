#ifndef NHPPP_INTENSITY_STEP_CORE_H
#define NHPPP_INTENSITY_STEP_CORE_H

#include <string>
#include "sc_step_core.h"

// Thinning (acceptance-rejection) sampler with a piecewise constant
// majorizer, templated over the grid policy. Unlike the sc_step cores this
// is not a special-case algorithm: the target intensity is an arbitrary
// (vectorized) R function `lambda`, and the majorizer only proposes
// candidate times that are accepted with probability
// lambda(t) / lambda_maj(interval of t).
//
// gen_at_least_K >= 1 draws the candidates from the majorizer CONDITIONED
// on >= K events (only the lower bound may be pushed into the proposal:
// {N_acc >= K} implies {N_maj >= K}, but an upper bound on N_maj would
// over-restrict and bias the accepted law). Conditioning the ACCEPTED
// count is the caller's job: the R-level rejection loop counts survivors
// per row against [gen_at_least_K, gen_at_most_K] and resamples the rows
// that fail. When gen_at_least_K >= 1 ALL accepted events are stored (the
// caller needs the full survivor count; reporting happens after
// convergence). In the unconditional case events are accepted in time
// order, so report_first_K may stop early and report_last_K compacts the
// row after the fact.

namespace nhppp {

template <class Grid>
Rcpp::NumericMatrix intensity_step_core(const Rcpp::Function& lambda,
                                        const Rcpp::NumericMatrix& rate_maj,
                                        const bool is_cumulative, const Grid& g,
                                        const Rcpp::NumericMatrix& subinterval,
                                        const double tol,
                                        const int report_first_K,
                                        const int report_last_K,
                                        const int gen_at_least_K,
                                        const int budget_cap) {
  const int n_intervals = rate_maj.cols();
  const int n_draws = rate_maj.rows();
  const double epsilon = std::numeric_limits<double>::epsilon();
  const bool conditioned = (gen_at_least_K >= 1);

  // Lambda_maj: cumulative majorizer at interval ends; lambda_maj: per-interval
  // rates. One of the two aliases rate_maj (read-only), the other is built.
  Rcpp::NumericMatrix Lambda_maj = build_Lambda(rate_maj, is_cumulative, g);
  Rcpp::NumericMatrix lambda_maj;
  if(!is_cumulative) {
    lambda_maj = rate_maj;
  } else {
    lambda_maj = Rcpp::NumericMatrix(n_draws, n_intervals);
    for(int draw = 0; draw != n_draws; ++draw){
      double prev = 0.0;
      for(int j = 0; j != n_intervals; ++j){
        lambda_maj(draw, j) = (rate_maj(draw, j) - prev) / g.width(draw, j);
        prev = rate_maj(draw, j);
      }
    }
  }

  // candidates: all majorizer events (no reporting cap — thinning must see them all)
  Rcpp::NumericMatrix Zstar =
      conditioned
          ? sc_step_orderstat_core(Lambda_maj, true, g, subinterval, tol,
                                   /*report_first_K=*/0, /*report_last_K=*/0,
                                   gen_at_least_K, /*gen_at_most_K=*/0, budget_cap)
          : sc_step_core(Lambda_maj, true, g, subinterval, tol,
                         /*report_first_K=*/0, /*report_last_K=*/0, budget_cap);

  Rcpp::NumericMatrix lambda_star = lambda(Zstar);

  Rcpp::NumericMatrix Z = na_matrix(n_draws, Zstar.cols());
  double acceptance_prob, f;
  int interval;
  int acc_i = 0;
  int max_acc_i = 0;

  for(int draw = 0; draw != n_draws; ++draw){
    acc_i = 0;
    for(int ev = 0; ev != Zstar.cols(); ++ev){
      if(Rcpp::NumericVector::is_na(Zstar(draw, ev))) {
        break;
      }
      interval = g.locate(draw, Zstar(draw, ev), f);
      acceptance_prob = (lambda_star(draw, ev)/lambda_maj(draw, interval));
      if(acceptance_prob > 1.0 + 5*epsilon || acceptance_prob < 0.0 - 5*epsilon) {
        std::string str = "Majorizer error? Pr(acceptance) = ";
        str += std::to_string(acceptance_prob);
        throw std::range_error(str);
      }

      if(acceptance_prob > (R::runif(0.0, 1.0))) {
        Z(draw, acc_i) = Zstar(draw, ev);
        ++acc_i;
        if(!conditioned && report_first_K > 0 && acc_i == report_first_K) {
          break;
        }
      }
    }
    if (!conditioned) {
      acc_i = compact_last(Z, draw, acc_i, report_last_K);
    }
    if (acc_i > 0) max_acc_i = std::max(max_acc_i, acc_i - 1);
  }

  return trim_columns(Z, max_acc_i, n_draws);
}

} // namespace nhppp

#endif
