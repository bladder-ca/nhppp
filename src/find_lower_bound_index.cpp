#include "nhppp.h"

// [[Rcpp::export]]
int find_lower_bound_index(const Rcpp::NumericVector& L, const int start,  const double tau) {
    auto it = std::lower_bound(L.begin() + start, L.end(), tau);
    if (it == L.end()) {
        return -1; // If tau is smaller than or equal to all elements of L
    } else {
        return std::distance(L.begin(), it); // Return the index
    }
}
