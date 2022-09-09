#include "nhppp_types.h"
using namespace Rcpp;



NumericVector big_lambda(const NumericVector & t){
  return (t*t);
}
double big_lambda(const double & t){
  return (t*t);
}

NumericVector small_lambda(const NumericVector & t){
  return (2.0*t);
}
double small_lambda(const double & t){
  return (2.0*t);
}

NumericVector big_lambda_inv(const NumericVector & z){
  return (sqrt(z));
}
double big_lambda_inv(const double & z){
  return (sqrt(z));
}



// The vignette L/lambdas  
double l_vignette(const double & t){
  const double r = 2.0;
  return exp(r*t) * (1 + sin(t));
}
double L_vignette(const double & t){
  const double r = 2.0;
  return (exp(r*t)*(r*sin(t) - cos(t)) + 1) / (1+r*r) + 
         (exp(r*t) - 1)/r ;
}

