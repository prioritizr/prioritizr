#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_bounded_constraints(SEXP x, Rcpp::IntegerVector pu,
                                    Rcpp::IntegerVector zone,
                                    Rcpp::NumericVector lower,
                                    Rcpp::NumericVector upper) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  // convert base-1 indexing to base-0 indexing
  pu = pu - 1;
  zone = zone - 1;
  // assign bounds based on indices
  for (auto i = 0; i < pu.size();  ++i) {
    ptr->_lb[(zone[i] * ptr->_number_of_planning_units) + pu[i]] = lower[i];
    ptr->_ub[(zone[i] * ptr->_number_of_planning_units) + pu[i]] = upper[i];
  }
  // return result
  return true;
}
