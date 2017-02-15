#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_add_binary_decision(SEXP x) {
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
    ptr->_vtype.push_back("B");
  for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
    ptr->_lb.push_back(0.0);
  for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
    ptr->_ub.push_back(1.0);
  return true;
}

// [[Rcpp::export]]
bool rcpp_add_proportion_decision(SEXP x) {
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
    ptr->_vtype.push_back("S");
  for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
    ptr->_lb.push_back(0.0);
  for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
    ptr->_ub.push_back(1.0);
  return true;
}

// [[Rcpp::export]]
bool rcpp_add_semicontinuous_decision(SEXP x, double upper) {
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
    ptr->_vtype.push_back("S");
  for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
    ptr->_lb.push_back(0.0);
  for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
    ptr->_ub.push_back(upper);
  return true;
}

