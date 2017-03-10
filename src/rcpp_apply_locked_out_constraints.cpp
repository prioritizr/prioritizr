#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_locked_out_constraints(SEXP x, Rcpp::IntegerVector indices) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  // assign bounds based on indices
  for (auto i=indices.begin(); i!=indices.end();  ++i)
    ptr->_ub[(*i) - 1] = 0.0;
  // return result
  return true;
}
