#ifndef HNPPP_H
#define HNPPP_H

#include <algorithm>
#include <cmath>

#include <Rcpp.h>
#include <Rmath.h>


void matrix_cumsum_columns_inplace(Rcpp::NumericMatrix & X);

void matrix_diff_columns_inplace(Rcpp::NumericMatrix & X);

Rcpp::NumericMatrix matrix_cumsum_columns(const Rcpp::NumericMatrix & X);

Rcpp::NumericMatrix matrix_diff_columns(const Rcpp::NumericMatrix & X);

int find_upper_bound_index(
  const Rcpp::NumericVector& L, 
  const int start, 
  const double tau);

int find_lower_bound_index(
  const Rcpp::NumericVector& L, 
  const int start, 
  const double tau);

double simple_lerp(
  const double a, 
  const double b, 
  const double f);

Rcpp::NumericMatrix vdraw_sc_step_regular2(
  const Rcpp::NumericMatrix & rate,
  const bool is_cumulative,
  const Rcpp::NumericMatrix & range_t,
  const Rcpp::NumericMatrix & subinterval,
  const double tol,
  const bool atmost1
);

Rcpp::NumericMatrix vdraw_sc_step_regular(
  const Rcpp::NumericMatrix & rate,
  const bool is_cumulative,
  const Rcpp::NumericMatrix & range_t,
  const double tol,
  const bool atmost1
);

Rcpp::NumericMatrix vztdraw_sc_step_regular(
  const Rcpp::NumericMatrix & rate,
  const bool is_cumulative,
  const Rcpp::NumericMatrix & range_t,
  const bool atmost1
);

Rcpp::NumericMatrix vztdraw_sc_step_regular2(
  const Rcpp::NumericMatrix & rate,
  const bool is_cumulative,
  const Rcpp::NumericMatrix & range_t,
  const Rcpp::NumericMatrix & subinterval,
  const bool atmost1
);

Rcpp::NumericMatrix vdraw_intensity_step_regular(
  Rcpp::Function lambda,
  const Rcpp::NumericMatrix & rate_maj,
  const bool is_cumulative,
  const Rcpp::NumericMatrix & range_t,
  const double tol,
  const bool atmost1);

Rcpp::NumericMatrix step_regular_inverse(
  int max_events, 
  const Rcpp::NumericMatrix & Lambda,
  const Rcpp::NumericMatrix & Tau, 
  const Rcpp::NumericMatrix & range_t, 
  bool atmost1
);


// class RegularStepInfo {
//   const bool atmost1 {false}; 
//   const int max_events {0}; 
//   const Rcpp::NumericMatrix & range_t {Rcpp::NumericMatrix(1, 1)};
//   const Rcpp::NumericVector & interval_duration {range_t(Rcpp::_,1) - range_t(Rcpp::_,0)};
//   Rcpp::NumericMatrix & lambda {Rcpp::NumericMatrix(1, 1)};
//   Rcpp::NumericMatrix & Lambda {matrix_cumsum_columns(lambda) * interval_duration};
// };

// Rcpp::NumericMatrix step_regular_inverse2(
//   const RegularStepInfo & info, 
//   const Rcpp::NumericMatrix & Tau
// );

int rztpois(const double lambda);

Rcpp::IntegerVector rztpois_vec(const Rcpp::NumericVector & lambda); 

#endif