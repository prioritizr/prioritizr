#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_binary_decisions(SEXP x) {
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  // determine how many variables to assign decisions to based on formulation
  std::size_t n;
  if (ptr->_compressed_formulation) {
    n = ptr->_number_of_planning_units;
  } else {
    n = ptr->_number_of_planning_units +
        (ptr->_number_of_planning_units * ptr->_number_of_features);
  }
  // assign decisions
  for (std::size_t i=0; i < n; ++i)
    ptr->_vtype.push_back("B");
  for (std::size_t i=0; i < n; ++i)
    ptr->_lb.push_back(0.0);
  for (std::size_t i=0; i < n; ++i)
    ptr->_ub.push_back(1.0);
  return true;
}

// [[Rcpp::export]]
bool rcpp_apply_proportion_decisions(SEXP x) {
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  // determine how many variables to assign decisions to based on formulation
  std::size_t n;
  if (ptr->_compressed_formulation) {
    n = ptr->_number_of_planning_units;
  } else {
    n = ptr->_number_of_planning_units +
        (ptr->_number_of_planning_units * ptr->_number_of_features);
  }
  // assign decisions
  for (std::size_t i=0; i < n; ++i)
    ptr->_vtype.push_back("C");
  for (std::size_t i=0; i < n; ++i)
    ptr->_lb.push_back(0.0);
  for (std::size_t i=0; i < n; ++i)
    ptr->_ub.push_back(1.0);
  return true;
}

// [[Rcpp::export]]
bool rcpp_apply_semicontinuous_decisions(SEXP x, double upper) {
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  // determine how many variables to assign decisions to based on formulation
  std::size_t n;
  if (ptr->_compressed_formulation) {
    n = ptr->_number_of_planning_units;
  } else {
    n = ptr->_number_of_planning_units +
        (ptr->_number_of_planning_units * ptr->_number_of_features);
  }
  // assign decisions
  for (std::size_t i=0; i < n; ++i)
    ptr->_vtype.push_back("C");
  for (std::size_t i=0; i < n; ++i)
    ptr->_lb.push_back(0.0);
  for (std::size_t i=0; i < n; ++i)
    ptr->_ub.push_back(upper);
  return true;
}
