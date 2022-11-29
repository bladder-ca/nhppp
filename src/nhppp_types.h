#include <Rcpp.h>
#include <Rmath.h>

typedef double (*funcPtr)(const double & x, const Rcpp::NumericVector params);

Rcpp::XPtr<funcPtr> putFunPtrInXPtr2(std::string fstr); 
double big_lambda(const double & t, const Rcpp::NumericVector params);
double small_lambda(const double & t, const Rcpp::NumericVector params);
double big_lambda_inv(const double & z, const Rcpp::NumericVector params);

double Lambda_linear_form(const double &t, const Rcpp::NumericVector params);
double Lambda_inv_linear_form(const double &z, const Rcpp::NumericVector params);


// the vignette L/lambdas 
double l_vignette(const double & t, const Rcpp::NumericVector params); 
double L_vignette(const double & t, const Rcpp::NumericVector params); 
