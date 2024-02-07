#include <Rmath.h>
#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;
void matrix_cumsum_columns_inplace(NumericMatrix & X);
void matrix_diff_columns_inplace(NumericMatrix & X);


int find_upper_bound_index(const NumericVector& L, const int start,  const double tau) {
    auto it = std::upper_bound(L.begin() + start, L.end(), tau);
    if (it == L.end()) {
        return -1; // If tau is greater than or equal to all elements of L
    } else {
        return std::distance(L.begin(), it); // Return the index
    }
}



// [[Rcpp::export]]
NumericMatrix vdraw_sc_step_regular(
  NumericMatrix & lambda,
  const bool is_cumulative,
  const NumericMatrix & range_t,
  const double tol,
  const bool atmost1
) {
  int n_intervals = lambda.cols();
  int n_draws = lambda.rows();
  NumericMatrix Lambda(n_draws, n_intervals);

  std::copy(lambda.begin(), lambda.end(), Lambda.begin());
  if(!is_cumulative) {
    matrix_cumsum_columns_inplace(Lambda);
  } else {
    matrix_diff_columns_inplace(lambda);
  }
  NumericVector interval_duration = (range_t.column(1) - range_t.column(0)) / n_intervals;

  int n_max_events = 1;
  if(!atmost1){
    n_max_events = R::qpois(1.0 - tol, max(Lambda), 1, 0);
    if(n_max_events == 0) {
      NumericMatrix Z(n_draws, 1); 
      std::fill( Z.begin(), Z.end(), NumericVector::get_na() ) ;
      return(Z);
    }
  }
  NumericMatrix Z(n_draws, n_max_events); 
  std::fill( Z.begin(), Z.end(), NumericVector::get_na() ) ;


  NumericMatrix Tau(n_draws, n_max_events);
  for(int i =0; i!=n_draws*n_max_events; ++i) {
    Tau[i] = R::rexp(1);
  }
  if(n_max_events>1){
    matrix_cumsum_columns_inplace(Tau);  
  }
    
  NumericVector L;
  int i1, i2, ev_max = 0;
  double L0;


  for(int draw = 0; draw != n_draws; ++draw){
    i1 = 0; 
    i2 = 0;
    L = Lambda.row(draw);
    for(int ev = 0; ev != n_max_events; ++ev){
      if(Tau(draw, ev) > L[n_intervals-1]) {
        break;
      }
      i2 = find_upper_bound_index(L, i1, Tau(draw, ev));
      if(i2 == -1) {
        break;
      }
      L0 = (i2>0) ? L[i2-1] : 0;

      Z(draw, ev) = range_t(draw, 0) + 
        interval_duration[draw] * (
          i2 +
          (Tau(draw, ev) - L0) / (L[i2] - L0)
        );
      i1 = i2;
      ev_max = std::max(ev_max, ev);
    }
  }
  return Z(Range(0, n_draws), Range(0, ev_max));
//  return Z;
}


