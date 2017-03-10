#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_neighbor_constraints(SEXP x,
                                    arma::sp_mat connected_matrix,
                                    int k) {

  /* The following code makes the following critical assumption
   *
   * connected_matrix is a sparse matrix with only cells in either the upper
   * or lower triangle and the diagonal filled in. If this condition is not met,
   * then constraints will be added incorrectly.
   */

  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t A_original_ncol = ptr->_obj.size();
  std::size_t A_original_nrow = ptr->_rhs.size();

  // extract data from the connected matrix
  std::vector<std::size_t> pu_i;
  pu_i.reserve(connected_matrix.n_nonzero);
  std::vector<std::size_t> pu_j;
  pu_j.reserve(connected_matrix.n_nonzero);
  std::size_t curr_i, curr_j;
  double curr_value;
  for (arma::sp_mat::const_iterator it=connected_matrix.begin();
       it!=connected_matrix.end();
       ++it) {
    // cell data
    curr_i = it.row();
    curr_j = it.col();
    curr_value = *it;
    // if the number indicating cell connections is greater than a very small
    // positive number then include it
    if (curr_value > 1.0e-10) {
      if (curr_i != curr_j) {
        // extract indices of connected planning units
        pu_i.push_back(curr_i);
        pu_j.push_back(curr_j);
      }
    }
  }

  // add constraints to specify that each planning unit should have
  // k number of neighbors
  k = k * -1;
  for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i) {
      ptr->_A_i.push_back(A_original_nrow + i);
      ptr->_A_j.push_back(i);
      ptr->_A_x.push_back(k);
      ptr->_sense.push_back(">=");
      ptr->_rhs.push_back(0);
      ptr->_row_ids.push_back("n");
  }

  // add constraints to specify the connected relationships
  for (std::size_t i=0; i<(pu_i.size()); ++i) {
    // constraint to ensure that pu_i is connected to pu_j
    ptr->_A_i.push_back(A_original_nrow + pu_i[i]);
    ptr->_A_j.push_back(pu_j[i]);
    ptr->_A_x.push_back(1.0);
  }

  // return result
  return true;
}
