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
    else
        return XPtr<funcPtr>(R_NilValue); // runtime error as NULL no XPtr
}

