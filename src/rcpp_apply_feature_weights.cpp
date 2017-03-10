#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_feature_weights(SEXP x, Rcpp::NumericVector weights) {
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_obj[i+ptr->_number_of_planning_units] = weights[i];
  return true;
}

