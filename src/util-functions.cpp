#include "nhppp_types.h"
using namespace Rcpp;


// [[Rcpp::export]]
XPtr<funcPtr> putFunPtrInXPtr2(std::string fstr) {
    if (fstr == "L")
        return(XPtr<funcPtr>(new funcPtr(&big_lambda)));
    else if (fstr == "Linv")
        return(XPtr<funcPtr>(new funcPtr(&big_lambda_inv)));
    else if (fstr == "l")
        return(XPtr<funcPtr>(new funcPtr(&small_lambda)));
    else if (fstr == "L_vignette")
        return(XPtr<funcPtr>(new funcPtr(&L_vignette)));
    else if (fstr == "l_vignette")
        return(XPtr<funcPtr>(new funcPtr(&l_vignette)));
    else if (fstr == "Lambda_linear_form")
        return(XPtr<funcPtr>(new funcPtr(&Lambda_linear_form)));
        else if (fstr == "Lambda_inv_linear_form")
        return(XPtr<funcPtr>(new funcPtr(&Lambda_inv_linear_form)));
    else
        return XPtr<funcPtr>(R_NilValue); // runtime error as NULL no XPtr
}

