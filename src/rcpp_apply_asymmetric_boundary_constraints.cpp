#include "package.h"
#include "optimization_problem.h"
#include "rcpp_boundary_data.h"

// [[Rcpp::export]]
bool rcpp_apply_asymmetric_boundary_constraints(SEXP x,
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
  typedef std::pair<std::size_t,std::size_t> PUPAIRID;
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t A_original_ncol = ptr->_obj.size();
  std::size_t A_original_nrow = ptr->_rhs.size();

  // apply penalty and edge scaling
  boundary_matrix *= penalty;
  boundary_matrix.diag() *= edge_factor;

  // extract data from the boundary matrix
  std::size_t curr_i, curr_j;
  std::vector<double> total_boundaries(ptr->_number_of_planning_units, 0.0);
  std::unordered_map<PUPAIRID, double, boost::hash<PUPAIRID>> edges;
  edges.reserve(boundary_matrix.n_nonzero - ptr->_number_of_planning_units);
  double curr_value;
  std::unordered_map<PUPAIRID, double, boost::hash<PUPAIRID>>::iterator curr_itr;
  PUPAIR curr_pu_pair;
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
        // extract planning unit indices and shared boundaries from the matrix
        curr_pu_pair = PUPAIR(curr_i, curr_j, curr_value);
        curr_itr = edges.find(curr_pu_pair._id);
        if (curr_itr == edges.end()) {
          edges.insert(std::pair<PUPAIRID, double>(curr_pu_pair._id, curr_value));
        } else {
          (curr_itr->second)+=curr_value;
        }
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
    for (auto itr=edges.cbegin(); itr!=edges.cend(); ++itr)
      ptr->_obj.push_back((itr->second) * -1.0);
  }

  // if the objective is to maximize the costs, then boundary penalties are
  // negative
  if (ptr->_modelsense == "max") {
    // add total boundaries to planning unit costs in obj
    for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
      ptr->_obj[i] -= total_boundaries[i];
    // add exposed boundaries to obj
    for (auto itr=edges.cbegin(); itr!=edges.cend(); ++itr)
      ptr->_obj.push_back((itr->second));
  }

  // add lb for new decision variables
  for (std::size_t i=0; i<(edges.size()); ++i)
    ptr->_lb.push_back(0.0);

  // add ub for new decision variables
  for (std::size_t i=0; i<(edges.size()); ++i)
    ptr->_ub.push_back(1.0);

  // add vtype for new decision variables
  for (std::size_t i=0; i<(edges.size()); ++i)
    ptr->_vtype.push_back(ptr->_vtype[0]);

  // add col ids for new decision variables
  for (std::size_t i=0; i<(edges.size()); ++i)
    ptr->_col_ids.push_back("b");

  // add new constraints to
  std::size_t i = 0;
  std::size_t A_row = (A_original_nrow-1);
  for (auto itr=edges.cbegin(); itr!=edges.cend(); ++itr) {
    // increment row
    ++A_row;

    // constraint to ensure that decision variable pu_i_j is less than or
    // equal to pu_i
    ptr->_A_i.push_back(A_row);
    ptr->_A_i.push_back(A_row);
    ptr->_A_j.push_back(A_original_ncol + i);
    ptr->_A_j.push_back(itr->first.first);
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
    ptr->_A_j.push_back(itr->first.second);
    ptr->_A_x.push_back(1.0);
    ptr->_A_x.push_back(-1.0);
    ptr->_sense.push_back("<=");
    ptr->_rhs.push_back(0.0);
    ptr->_row_ids.push_back("b2");

//       // constraint to ensure that decision variable pu_i_j is calculated
//       // correctly. This is not needed if the pu_i and pu_j decision variables
//       // are binary.
//
//       ++A_row;
//       ptr->_A_i.push_back(A_row);
//       ptr->_A_i.push_back(A_row);
//       ptr->_A_i.push_back(A_row);
//       ptr->_A_j.push_back(A_original_ncol + i);
//       ptr->_A_j.push_back(itr->first.first);
//       ptr->_A_j.push_back(itr->first.second);
//       ptr->_A_x.push_back(1.0);
//       ptr->_A_x.push_back(-1.0);
//       ptr->_A_x.push_back(-1.0);
//       ptr->_sense.push_back(">=");
//       ptr->_rhs.push_back(-1.0);
//       ptr->_row_ids.push_back("b3");

    // increment counter
    ++i;

  }

  // return result
  return true;
}
