#include "package.h"
#include "optimization_problem.h"
#include "rcpp_boundary_data.h"

// [[Rcpp::export]]
bool rcpp_apply_asymmetric_connectivity_constraints(SEXP x,
  Rcpp::List connectivity_data, double penalty) {
  // return result
  return false;
}
