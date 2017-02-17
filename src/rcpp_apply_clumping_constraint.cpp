#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_clumping_constraint(SEXP x, Rcpp::DataFrame boundary_matrix, double penalty, double edge_factor) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  // return error
  Rcpp::stop("not implemeted");
  // return result
  return true;
}
