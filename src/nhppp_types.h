#include <Rcpp.h>
#include <Rmath.h>

typedef double (*funcPtr)(const double & x);

Rcpp::XPtr<funcPtr> putFunPtrInXPtr2(std::string fstr); 
Rcpp::NumericVector big_lambda(const Rcpp::NumericVector & t);
double big_lambda(const double & t);
Rcpp::NumericVector small_lambda(const Rcpp::NumericVector & t);
double small_lambda(const double & t);
Rcpp::NumericVector big_lambda_inv(const Rcpp::NumericVector & z);
double big_lambda_inv(const double & z);

// the vignette L/lambdas 
double l_vignette(const double & t); 
double L_vignette(const double & t); 
