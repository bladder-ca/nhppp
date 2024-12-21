#ifndef HNPPP_H
#define HNPPP_H

#include <algorithm>
#include <cmath>
#include <limits>

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
  const bool atmost1, 
  const int atmostB
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
  const Rcpp::Function & lambda,
  const Rcpp::NumericMatrix & rate_maj,
  const bool is_cumulative,
  const Rcpp::NumericMatrix & range_t,
  const Rcpp::NumericMatrix & subinterval,
  const bool use_subinteval,
  const double tol,
  const bool atmost1, 
  const int atmostB);


Rcpp::NumericMatrix step_regular_inverse(
  const int max_events, 
  const Rcpp::NumericMatrix & Lambda,
  const Rcpp::NumericMatrix & Tau, 
  const Rcpp::NumericMatrix & range_t, 
  const bool atmost1
);

int safe_double_to_int(const double x);

int rztpois(const double lambda);

Rcpp::IntegerVector rztpois_vec(const Rcpp::NumericVector & lambda); 

#endif
