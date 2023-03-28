#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_linear_constraints(
  SEXP x, double threshold, std::string sense, const arma::sp_mat data) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t i = ptr->_rhs.size();

  // add model coefficients
  for (auto itr = data.begin(); itr != data.end(); ++itr) {
    ptr->_A_i.push_back(i);
    ptr->_A_j.push_back((itr.col() * ptr->_number_of_planning_units) +
                        itr.row());
    ptr->_A_x.push_back(*itr);
  }

  // add sense
  ptr->_sense.push_back(sense);

  // add rhs
  ptr->_rhs.push_back(threshold);

  // add row id
  ptr->_row_ids.push_back("lc");

  // return success
  return true;
}
