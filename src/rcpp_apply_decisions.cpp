#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_decisions(SEXP x, std::string vtype, double default_lower,
                          double default_upper) {
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  // determine how many variables to assign decisions to based on formulation
  std::size_t n = ptr->_number_of_planning_units * ptr->_number_of_zones;
  if (!ptr->_compressed_formulation) {
    n += (ptr->_number_of_zones * ptr->_number_of_planning_units *
          ptr->_number_of_features);
  }
  // assign decisions
  for (std::size_t i=0; i < n; ++i)
    ptr->_vtype.push_back(vtype);
  for (std::size_t i=0; i < n; ++i)
    ptr->_lb.push_back(default_lower);
  for (std::size_t i=0; i < n; ++i)
    ptr->_ub.push_back(default_upper);
  // return success
  return true;
}
