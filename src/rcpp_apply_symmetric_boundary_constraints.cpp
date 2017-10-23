#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_symmetric_boundary_constraints(SEXP x,
                                    arma::sp_mat boundary_matrix,
                                    double penalty,
                                    double edge_factor) {

  /* The following code makes the following critical assumption
   *
   * boundary_matrix is a sparse matrix with only cells in either the upper
   * or lower triangle and the diagonal filled in. If this condition is not met,
   * then the boundary calculations will be incorrect.
   */

  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t A_original_ncol = ptr->_obj.size();
  std::size_t A_original_nrow = ptr->_rhs.size();

  // apply penalty and edge scaling
  boundary_matrix *= penalty;
  boundary_matrix.diag() *= edge_factor;

  // extract data from the boundary matrix
  std::vector<double> total_boundaries(ptr->_number_of_planning_units, 0.0);
  std::vector<std::size_t> pu_i;
  pu_i.reserve(boundary_matrix.n_nonzero);
  std::vector<std::size_t> pu_j;
  pu_j.reserve(boundary_matrix.n_nonzero);
  std::vector<double> pu_b;
  pu_b.reserve(boundary_matrix.n_nonzero);
  std::size_t curr_i, curr_j;
  double curr_value;
  for (arma::sp_mat::const_iterator it=boundary_matrix.begin();
       it!=boundary_matrix.end();
       ++it) {
    // get row and column indices for cell
    curr_i = it.row();
    curr_j = it.col();
    curr_value = *it;
    if (curr_value > 1.0e-10) {
      // add the boundary to the total for planning unit i
      total_boundaries[curr_i] += curr_value;
      if (curr_i != curr_j) {
        // add the boundary to the total for planning unit j
        // this is done when i!=j to ensure that we aren't adding the exposed
        // boundary twice
        total_boundaries[curr_j] += curr_value;
        // extract planning unit indices and shared boundaries from the matrix
        pu_i.push_back(curr_i);
        pu_j.push_back(curr_j);
        pu_b.push_back(curr_value);
      }
    }
  }


  // if the objective is to minimize the costs, then boundary penalties are
  // positive
  if (ptr->_modelsense == "min") {
    // add total boundaries to planning unit costs in obj
    for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
      ptr->_obj[i] += total_boundaries[i];
    // add exposed boundaries to obj
    for (auto i=pu_b.cbegin(); i!=pu_b.cend(); ++i)
      ptr->_obj.push_back((*i) * -2.0);
  }

  // if the objective is to maximize the costs, then boundary penalties are
  // negative
  if (ptr->_modelsense == "max") {
    // add total boundaries to planning unit costs in obj
    for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
      ptr->_obj[i] -= total_boundaries[i];
    // add exposed boundaries to obj
    for (auto i=pu_b.cbegin(); i!=pu_b.cend(); ++i)
      ptr->_obj.push_back((*i) * 2.0);
  }

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
    ptr->_col_ids.push_back("b");

  // add new constraints to
  std::size_t A_row = (A_original_nrow-1);
  for (std::size_t i=0; i<(pu_i.size()); ++i) {
    // increment row
    ++A_row;
    // constraint to ensure that decision variable pu_i_j is less than or
    // equal to pu_i
    ptr->_A_i.push_back(A_row);
    ptr->_A_i.push_back(A_row);
    ptr->_A_j.push_back(A_original_ncol + i);
    ptr->_A_j.push_back(pu_i[i]);
    ptr->_A_x.push_back(1.0);
    ptr->_A_x.push_back(-1.0);
    ptr->_sense.push_back("<=");
    ptr->_rhs.push_back(0.0);
    ptr->_row_ids.push_back("b1");

    // constraint to ensure that decision variable pu_i_j is less than or
    // equal to pu_j
    ++A_row;
    ptr->_A_i.push_back(A_row);
    ptr->_A_i.push_back(A_row);
    ptr->_A_j.push_back(A_original_ncol + i);
    ptr->_A_j.push_back(pu_j[i]);
    ptr->_A_x.push_back(1.0);
    ptr->_A_x.push_back(-1.0);
    ptr->_sense.push_back("<=");
    ptr->_rhs.push_back(0.0);
    ptr->_row_ids.push_back("b2");

//     if (ptr->_vtype[0] != "B") {
//
//       // constraint to ensure that decision variable pu_i_j is calculated
//       // correctly. This is not needed if the pu_i and pu_j decision variables
//       // are binary.
//
//       ++A_row;
//       ptr->_A_i.push_back(A_row);
//       ptr->_A_i.push_back(A_row);
//       ptr->_A_i.push_back(A_row);
//       ptr->_A_j.push_back(A_original_ncol + i);
//       ptr->_A_j.push_back(pu_i[i]);
//       ptr->_A_j.push_back(pu_j[i]);
//       ptr->_A_x.push_back(1.0);
//       ptr->_A_x.push_back(-1.0);
//       ptr->_A_x.push_back(-1.0);
//       ptr->_sense.push_back(">=");
//       ptr->_rhs.push_back(-1.0);
//       ptr->_row_ids.push_back("b3");
//
//     }

  }

  // return result
  return true;
}
