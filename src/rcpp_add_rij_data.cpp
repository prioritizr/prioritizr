#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_add_rij_data(SEXP x, Rcpp::DataFrame rij, Rcpp::IntegerVector dim) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  // assign indices
  ptr->_number_of_features = static_cast<std::size_t>(dim[0]);
  ptr->_number_of_planning_units = static_cast<std::size_t>(dim[1]);
  // extract columns from dataframe
  Rcpp::IntegerVector planning_units = rij["i"];
  Rcpp::IntegerVector features = rij["j"];
  Rcpp::IntegerVector values = rij["x"];  
  // assign decision matrix variables
  for (auto i=planning_units.begin(); i!=planning_units.end(); ++i)
    ptr->_A_i.push_back(*i);
  for (auto i=features.begin(); i!=features.end(); ++i)
    ptr->_A_j.push_back(*i);
  for (auto i=values.begin(); i!=values.end(); ++i)
    ptr->_A_x.push_back(*i);
  // return result
  return true;
}
