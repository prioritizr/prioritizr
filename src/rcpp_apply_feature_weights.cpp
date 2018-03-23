#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_feature_weights(SEXP x, Rcpp::NumericVector weights) {
  // initialize
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  const std::size_t n_weights = weights.size();
  std::size_t A_extra_ncol = ptr->_number_of_planning_units;
  if (!ptr->_compressed_formulation)
    A_extra_ncol += (ptr->_number_of_planning_units * ptr->_number_of_features);
  // add weights
  for (std::size_t i = 0; i < n_weights; ++i)
    ptr->_obj[A_extra_ncol + i] = weights[i];
  // return success
  return true;
}
