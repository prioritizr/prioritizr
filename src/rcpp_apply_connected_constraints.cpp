#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_connected_constraints(SEXP x, arma::sp_mat connected_matrix) {
  /* The following code makes the following critical assumption
   *
   * connected_matrix is a sparse matrix with only cells in either the upper
   * or lower triangle and the diagonal filled in. If this condition is not met,
   * then the connection calculations will be incorrect.
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
    // get row and column indices for cell
    curr_i = it.row();
    curr_j = it.col();
    curr_value = *it;
    if (curr_value > 1.0e-10) {
      if (curr_i != curr_j) {
        // extract connected planning unit from the matrix
        pu_i.push_back(curr_i);
        pu_j.push_back(curr_j);
      }
    }
  }

  // add in zeros to the objective function for the new decision variables
  for (auto i = pu_i.cbegin(); i != pu_i.cend(); ++i)
    ptr->_obj.push_back(0.0);

  // add lb for new decision variables
  for (auto i=pu_i.cbegin(); i!=pu_i.cend(); ++i)
    ptr->_lb.push_back(0.0);

  // add ub for new decision variables
  for (auto i=pu_i.cbegin(); i!=pu_i.cend(); ++i)
    ptr->_ub.push_back(1.0);

  // add vtype for new decision variables
  for (auto i=pu_i.cbegin(); i!=pu_i.cend(); ++i)
    ptr->_vtype.push_back(ptr->_vtype[0]);

  // add col ids for new decision variables
  for (auto i=pu_i.cbegin(); i!=pu_i.cend(); ++i)
    ptr->_col_ids.push_back("c");

  // add new constraints to ensure selected planning units are connected
  std::size_t A_row = (A_original_nrow-1);
  for (std::size_t i=0; i<(pu_i.size()); ++i) {
    // increment row
    ++A_row;
    // constraint to ensure that pu_ij <= pu_i
    ptr->_A_i.push_back(A_row);
    ptr->_A_i.push_back(A_row);
    ptr->_A_j.push_back(A_original_ncol + i);
    ptr->_A_j.push_back(pu_i[i]);
    ptr->_A_x.push_back(1.0);
    ptr->_A_x.push_back(-1.0);
    ptr->_sense.push_back("<=");
    ptr->_rhs.push_back(0.0);
    ptr->_row_ids.push_back("c1");
  }

  // constraint to ensure that \sum_{i \in neighbors of j} pu_ij <= pu_j
  // this ensure that if multiple connections are going to pu_j that
  // only one connection is active
  std::size_t current_pu_j = ptr->_number_of_planning_units + 10;
  for (std::size_t i=0; i<(pu_i.size()); ++i) {
    if (pu_j[i] != current_pu_j) {
      ++A_row;
      current_pu_j = pu_j[i];
      ptr->_A_i.push_back(A_row);
      ptr->_A_j.push_back(current_pu_j);
      ptr->_A_x.push_back(-1.0);
      ptr->_sense.push_back("<=");
      ptr->_row_ids.push_back("c2");
      ptr->_rhs.push_back(0.0);
    }
    ptr->_A_i.push_back(A_row);
    ptr->_A_j.push_back(A_original_ncol + i);
    ptr->_A_x.push_back(1.0);
  }

  // add constraints to ensure that the number of active connections between
  // planning units is equal to the number of selected planning units minus one.
  // this constraint ensures that the solution is actually connected
  ++A_row;
  for (std::size_t i = 0; i < (ptr->_number_of_planning_units); ++i) {
    // add planning unit values
    ptr->_A_i.push_back(A_row);
    ptr->_A_j.push_back(i);
    ptr->_A_x.push_back(-1.0);
  }
  for (std::size_t i = 0; i < (pu_i.size()); ++i) {
    // add connection values
    ptr->_A_i.push_back(A_row);
    ptr->_A_j.push_back(A_original_ncol + i);
    ptr->_A_x.push_back(1.0);
  }
  ptr->_sense.push_back("=");
  ptr->_rhs.push_back(-1);
  ptr->_row_ids.push_back("c3");

  // return result
  return true;
}
