#ifndef HNPPP_H
#define HNPPP_H

#include <Rcpp.h>
#include <Rmath.h>
#include <algorithm>


void matrix_cumsum_columns_inplace(Rcpp::NumericMatrix & X);

void matrix_diff_columns_inplace(Rcpp::NumericMatrix & X);

Rcpp::NumericMatrix matrix_cumsum_columns(const Rcpp::NumericMatrix & X);

Rcpp::NumericMatrix matrix_diff_columns(const Rcpp::NumericMatrix & X);

int find_upper_bound_index(
  const Rcpp::NumericVector& L, 
  const int start, 
  const double tau);

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

int rztpois(const double lambda);

Rcpp::IntegerVector rztpois_vec(const Rcpp::NumericVector & lambda); 

#endif