#include "nhppp_types.h"
using namespace Rcpp;


double big_lambda(const double & t, const Rcpp::NumericVector params = (0.0)){
  return (t*t);
}

double small_lambda(const double & t, const Rcpp::NumericVector params = (0.0)){
  return (2.0*t);
}

double big_lambda_inv(const double & z, const Rcpp::NumericVector params = (0.0)){
  return (sqrt(z));
}




double Lambda_linear_form(const double &t, const Rcpp::NumericVector params = (0.0, 1.0, 0.0)) {
  double alpha = params[0];
  double beta = params[1];
  double t0 = params[2];
  double t1 = t; 
  if (t < t0) {
    stop("Error: t < params[2] == t0");
  }
  if (beta < 0 && t1 > -alpha / beta) {
    t1 = -alpha / beta;
  }
  return alpha * (t1 - t0) + (beta / 2) * (t1*t1 - t0*t0);
}


double Lambda_inv_linear_form(const double &z, const Rcpp::NumericVector params = (0.0, 1.0, 0.0)) {
  double alpha = params[0];
  double beta = params[1];
  double t0 = params[2];
  double L0, Delta, t = 0.0;

  if(beta == 0 && alpha == 0) {
    stop("Error: alpha and beta both 0"); 
  }
  if (beta != 0) {
    L0 = -alpha * t0 - beta / 2 * t0 * t0;
    Delta = alpha*alpha   - 2 * beta * (L0 - z);
    if(Delta < 0) {
      stop("Error: Delta <0");
    }
    t = (-alpha + sqrt(Delta)) / beta;
  } else if (beta == 0) {
    t = z / alpha + t0;
  }
  return t;
}




// The vignette L/lambdas  
double l_vignette(const double & t, const Rcpp::NumericVector params = (2.0)){
  const double r = params[0];
  return exp(r*t) * (1 + sin(t));
}
double L_vignette(const double & t, const Rcpp::NumericVector params = (2.0)){
  const double r = params[0];
  return (exp(r*t)*(r*sin(t) - cos(t)) + 1) / (1+r*r) + 
         (exp(r*t) - 1)/r ;
}

