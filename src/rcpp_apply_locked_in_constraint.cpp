#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_locked_in_constraint(SEXP x, Rcpp::IntegerVector indices) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  // assign bounds based on indices
  for (auto i=indices.begin(); i!=indices.end();  ++i)
    ptr->_lb[(*i) - 1] = 1.0;
  // return result
  return true;
}

