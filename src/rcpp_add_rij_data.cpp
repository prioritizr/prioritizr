#include <RcppArmadillo.h>
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_add_rij_data(SEXP x, arma::sp_mat rij) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  // assign indices
  ptr->_number_of_features = static_cast<std::size_t>(rij.n_rows);
  ptr->_number_of_planning_units = static_cast<std::size_t>(rij.n_cols);
  // assign decision matrix variables
  for (auto it=rij.begin(); it!=rij.end(); ++it) {
    ptr->_A_i.push_back(it.row());
    ptr->_A_j.push_back(it.col());  
    ptr->_A_x.push_back(*it);
  }
  // return result
  return true;
}
