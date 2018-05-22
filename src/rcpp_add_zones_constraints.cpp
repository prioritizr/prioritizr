#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_add_zones_constraints(SEXP x) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t A_original_ncol = ptr->_obj.size();
  std::size_t A_original_nrow = ptr->_rhs.size();
  // add constraints to ensure that the total proportion allocated to each zone
  // not exceed one. For binary decisions, this also ensures that each
  // planning unit is only allocated to a single zone.
  for (std::size_t j = 0; j < (ptr->_number_of_planning_units); ++j) {
    for (std::size_t z = 0; z < (ptr->_number_of_zones); ++z)
      ptr->_A_i.push_back(A_original_nrow + j);
    for (std::size_t z = 0; z < (ptr->_number_of_zones); ++z)
      ptr->_A_j.push_back((z * ptr->_number_of_planning_units) + j);
    for (std::size_t z = 0; z < (ptr->_number_of_zones); ++z)
      ptr->_A_x.push_back(1.0);
    ptr->_sense.push_back("<=");
    ptr->_rhs.push_back(1.0);
    ptr->_row_ids.push_back("pu_zone");
  }
  // return result
  return true;
}
