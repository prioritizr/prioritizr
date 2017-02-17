#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_locked_out_constraint(SEXP x, Rcpp::IntegerVector status) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  // assign bounds based on statuses
  for (std::size_t i=0; i<ptr->_number_of_planning_units;  ++i) {
    if (status[i]==1)
      ptr->_ub[i] = 0.0;
  }
  // return result
  return true;
}
