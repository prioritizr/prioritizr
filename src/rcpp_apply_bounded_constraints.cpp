#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_bounded_constraints(SEXP x,
                                    const Rcpp::IntegerVector pu,
                                    const Rcpp::IntegerVector zone,
                                    const Rcpp::NumericVector lower,
                                    const Rcpp::NumericVector upper) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  // create temporary variables
  std::size_t idx;
  // assign bounds based on indices
  for (auto i = 0; i < pu.size();  ++i) {
    idx = ((zone[i] - 1) * ptr->_number_of_planning_units) + (pu[i] - 1);
    ptr->_lb[idx] = lower[i];
    ptr->_ub[idx] = upper[i];
  }
  // return result
  return true;
}
