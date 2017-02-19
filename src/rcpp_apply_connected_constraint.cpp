#include "prioritizr.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_connected_constraint(SEXP x, Rcpp::DataFrame boundary_matrix) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  // return error
  Rcpp::stop("not implemeted");
  // return result
  return true;
}

