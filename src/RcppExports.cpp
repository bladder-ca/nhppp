// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "nhppp_types.h"
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// sim_ppp_cn
NumericVector sim_ppp_cn(const double rate, const int n, const double t_min);
RcppExport SEXP _nhppp_sim_ppp_cn(SEXP rateSEXP, SEXP nSEXP, SEXP t_minSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const double >::type rate(rateSEXP);
    Rcpp::traits::input_parameter< const int >::type n(nSEXP);
    Rcpp::traits::input_parameter< const double >::type t_min(t_minSEXP);
    rcpp_result_gen = Rcpp::wrap(sim_ppp_cn(rate, n, t_min));
    return rcpp_result_gen;
END_RCPP
}
// sim_ppp_ct
NumericVector sim_ppp_ct(const double rate, const double t_min, const double t_max, const double tol, const bool only1);
RcppExport SEXP _nhppp_sim_ppp_ct(SEXP rateSEXP, SEXP t_minSEXP, SEXP t_maxSEXP, SEXP tolSEXP, SEXP only1SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const double >::type rate(rateSEXP);
    Rcpp::traits::input_parameter< const double >::type t_min(t_minSEXP);
    Rcpp::traits::input_parameter< const double >::type t_max(t_maxSEXP);
    Rcpp::traits::input_parameter< const double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< const bool >::type only1(only1SEXP);
    rcpp_result_gen = Rcpp::wrap(sim_ppp_ct(rate, t_min, t_max, tol, only1));
    return rcpp_result_gen;
END_RCPP
}
// sim_nhppp_ct_inv
NumericVector sim_nhppp_ct_inv(const double t_min, const double t_max, std::string L_str, NumericVector L_params, std::string L_inv_str, NumericVector L_inv_params, bool only1);
RcppExport SEXP _nhppp_sim_nhppp_ct_inv(SEXP t_minSEXP, SEXP t_maxSEXP, SEXP L_strSEXP, SEXP L_paramsSEXP, SEXP L_inv_strSEXP, SEXP L_inv_paramsSEXP, SEXP only1SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const double >::type t_min(t_minSEXP);
    Rcpp::traits::input_parameter< const double >::type t_max(t_maxSEXP);
    Rcpp::traits::input_parameter< std::string >::type L_str(L_strSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type L_params(L_paramsSEXP);
    Rcpp::traits::input_parameter< std::string >::type L_inv_str(L_inv_strSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type L_inv_params(L_inv_paramsSEXP);
    Rcpp::traits::input_parameter< bool >::type only1(only1SEXP);
    rcpp_result_gen = Rcpp::wrap(sim_nhppp_ct_inv(t_min, t_max, L_str, L_params, L_inv_str, L_inv_params, only1));
    return rcpp_result_gen;
END_RCPP
}
// sim_nhppp_ct_linear
NumericVector sim_nhppp_ct_linear(const double alpha, const double beta, const double t_min, double t_max, const double tol, const bool only1);
RcppExport SEXP _nhppp_sim_nhppp_ct_linear(SEXP alphaSEXP, SEXP betaSEXP, SEXP t_minSEXP, SEXP t_maxSEXP, SEXP tolSEXP, SEXP only1SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< const double >::type t_min(t_minSEXP);
    Rcpp::traits::input_parameter< double >::type t_max(t_maxSEXP);
    Rcpp::traits::input_parameter< const double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< const bool >::type only1(only1SEXP);
    rcpp_result_gen = Rcpp::wrap(sim_nhppp_ct_linear(alpha, beta, t_min, t_max, tol, only1));
    return rcpp_result_gen;
END_RCPP
}
// sim_nhppp_ct_thinning_1
NumericVector sim_nhppp_ct_thinning_1(const double t_min, const double t_max, std::string l_str, NumericVector l_params, NumericVector l_maj_params, const double tol, bool only1);
RcppExport SEXP _nhppp_sim_nhppp_ct_thinning_1(SEXP t_minSEXP, SEXP t_maxSEXP, SEXP l_strSEXP, SEXP l_paramsSEXP, SEXP l_maj_paramsSEXP, SEXP tolSEXP, SEXP only1SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const double >::type t_min(t_minSEXP);
    Rcpp::traits::input_parameter< const double >::type t_max(t_maxSEXP);
    Rcpp::traits::input_parameter< std::string >::type l_str(l_strSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type l_params(l_paramsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type l_maj_params(l_maj_paramsSEXP);
    Rcpp::traits::input_parameter< const double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< bool >::type only1(only1SEXP);
    rcpp_result_gen = Rcpp::wrap(sim_nhppp_ct_thinning_1(t_min, t_max, l_str, l_params, l_maj_params, tol, only1));
    return rcpp_result_gen;
END_RCPP
}
// sim_nhppp_ct_thinning
NumericVector sim_nhppp_ct_thinning(const double t_min, const double t_max, std::string l_str, NumericVector l_params, NumericVector l_maj_params, const double tol, bool only1);
RcppExport SEXP _nhppp_sim_nhppp_ct_thinning(SEXP t_minSEXP, SEXP t_maxSEXP, SEXP l_strSEXP, SEXP l_paramsSEXP, SEXP l_maj_paramsSEXP, SEXP tolSEXP, SEXP only1SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const double >::type t_min(t_minSEXP);
    Rcpp::traits::input_parameter< const double >::type t_max(t_maxSEXP);
    Rcpp::traits::input_parameter< std::string >::type l_str(l_strSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type l_params(l_paramsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type l_maj_params(l_maj_paramsSEXP);
    Rcpp::traits::input_parameter< const double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< bool >::type only1(only1SEXP);
    rcpp_result_gen = Rcpp::wrap(sim_nhppp_ct_thinning(t_min, t_max, l_str, l_params, l_maj_params, tol, only1));
    return rcpp_result_gen;
END_RCPP
}
// putFunPtrInXPtr2
XPtr<funcPtr> putFunPtrInXPtr2(std::string fstr);
RcppExport SEXP _nhppp_putFunPtrInXPtr2(SEXP fstrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type fstr(fstrSEXP);
    rcpp_result_gen = Rcpp::wrap(putFunPtrInXPtr2(fstr));
    return rcpp_result_gen;
END_RCPP
}
// Lambda_lf
NumericVector Lambda_lf(const Rcpp::NumericVector t, const Rcpp::NumericVector params);
RcppExport SEXP _nhppp_Lambda_lf(SEXP tSEXP, SEXP paramsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector >::type t(tSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector >::type params(paramsSEXP);
    rcpp_result_gen = Rcpp::wrap(Lambda_lf(t, params));
    return rcpp_result_gen;
END_RCPP
}
// Lambda_inv_lf
NumericVector Lambda_inv_lf(const Rcpp::NumericVector z, const Rcpp::NumericVector params);
RcppExport SEXP _nhppp_Lambda_inv_lf(SEXP zSEXP, SEXP paramsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector >::type z(zSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector >::type params(paramsSEXP);
    rcpp_result_gen = Rcpp::wrap(Lambda_inv_lf(z, params));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_nhppp_sim_ppp_cn", (DL_FUNC) &_nhppp_sim_ppp_cn, 3},
    {"_nhppp_sim_ppp_ct", (DL_FUNC) &_nhppp_sim_ppp_ct, 5},
    {"_nhppp_sim_nhppp_ct_inv", (DL_FUNC) &_nhppp_sim_nhppp_ct_inv, 7},
    {"_nhppp_sim_nhppp_ct_linear", (DL_FUNC) &_nhppp_sim_nhppp_ct_linear, 6},
    {"_nhppp_sim_nhppp_ct_thinning_1", (DL_FUNC) &_nhppp_sim_nhppp_ct_thinning_1, 7},
    {"_nhppp_sim_nhppp_ct_thinning", (DL_FUNC) &_nhppp_sim_nhppp_ct_thinning, 7},
    {"_nhppp_putFunPtrInXPtr2", (DL_FUNC) &_nhppp_putFunPtrInXPtr2, 1},
    {"_nhppp_Lambda_lf", (DL_FUNC) &_nhppp_Lambda_lf, 2},
    {"_nhppp_Lambda_inv_lf", (DL_FUNC) &_nhppp_Lambda_inv_lf, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_nhppp(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
