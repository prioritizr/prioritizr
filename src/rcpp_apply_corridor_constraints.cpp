#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_corridor_constraints(SEXP x, Rcpp::List connected_matrix_list,
                                     Rcpp::NumericVector threshold) {
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

  // check that expanded formulation of the base conservation problem is used
  if (ptr->_compressed_formulation)
    Rcpp::stop("the compressed formulation cannot be used with this constraint.");

  // declare variables
  std::size_t A_feature_column_start = A_original_ncol;
  std::size_t A_row = (A_original_nrow-1);
  arma::sp_mat connected_matrix;
  std::vector<std::size_t> pu_i;
  std::vector<std::size_t> pu_j;
  std::size_t curr_i, curr_j, current_pu_j;

  // iterate over features and add constraints
  for (std::size_t f = 0; f < connected_matrix_list.size(); ++f) {
    // clear data from objects
    pu_i.clear();
    pu_j.clear();

    // extract connected matrix from list
    connected_matrix = Rcpp::as<arma::sp_mat>(connected_matrix_list[f]);

    // preallocate vectors
    pu_i.reserve(connected_matrix.n_nonzero);
    pu_j.reserve(connected_matrix.n_nonzero);

    // extract data from the connected matrix
    for (arma::sp_mat::const_iterator itr = connected_matrix.begin();
         itr!=connected_matrix.end();
         ++itr) {
      // get row and column indices for cell
      curr_i = itr.row();
      curr_j = itr.col();
      if ( (*itr >= threshold[f]) & (curr_i != curr_j)) {
        // extract connection from matrix if the strength of connectivity
        // is above the threshold
        pu_i.push_back(curr_i);
        pu_j.push_back(curr_j);
      }
    }

    // add constraints to ensure that all active connections are only
    // leaving selected planning units used to represent species f
    for (std::size_t i = 0; i < (pu_i.size()); ++i) {
      // increment row
      ++A_row;
      // constraint to ensure that con_ijk <= y_ij
      ptr->_A_i.push_back(A_row);
      ptr->_A_i.push_back(A_row);
      ptr->_A_j.push_back(A_feature_column_start + i);
      ptr->_A_j.push_back(ptr->_number_of_planning_units +
                          (f * ptr->_number_of_planning_units) + pu_i[i]);
      ptr->_A_x.push_back(1.0);
      ptr->_A_x.push_back(-1.0);
      ptr->_sense.push_back("<=");
      ptr->_rhs.push_back(0.0);
      ptr->_row_ids.push_back("c1");
    }

    // constraint to ensure that \sum_{i \in neighbors of j} pu_ijk <= y_ij
    // this ensure that if multiple connections are going to y_ij that
    // only one connection is active
    current_pu_j = ptr->_number_of_planning_units + 10;
    for (std::size_t i = 0; i < (pu_i.size()); ++i) {
      if (pu_j[i] != current_pu_j) {
        ++A_row;
        current_pu_j = pu_j[i];
        ptr->_A_i.push_back(A_row);
        ptr->_A_j.push_back(ptr->_number_of_planning_units +
                            (f * ptr->_number_of_planning_units) +
                            current_pu_j);
        ptr->_A_x.push_back(-1.0);
        ptr->_sense.push_back("<=");
        ptr->_row_ids.push_back("c2");
        ptr->_rhs.push_back(0.0);
      }
      ptr->_A_i.push_back(A_row);
      ptr->_A_j.push_back(A_feature_column_start + i);
      ptr->_A_x.push_back(1.0);
    }

    // add constraints to ensure that the number of active connections between
    // planning units used to represent each feature is equal to the number of
    // selected planning units that have connections. ie. ensure that the
    // all selected planning units used to represent a given feature are
    // connected.

    // add constraints
    ++A_row;
    for (std::size_t i = 0; i < (pu_i.size()); ++i) {
      ptr->_A_i.push_back(A_row);
      ptr->_A_j.push_back(A_feature_column_start + i);
      ptr->_A_x.push_back(1.0);
    }
    for (std::size_t i = 0; i < (ptr->_number_of_planning_units); ++i) {
      ptr->_A_i.push_back(A_row);
      ptr->_A_j.push_back(ptr->_number_of_planning_units +
                          (f * ptr->_number_of_planning_units) + i);
      ptr->_A_x.push_back(-1.0);
    }
    ptr->_sense.push_back("=");
    ptr->_rhs.push_back(-1);
    ptr->_row_ids.push_back("c3");

    // update number of columns so that the connection variables for each
    // feature are different
    A_feature_column_start += pu_i.size();
  }

  // add in zeros to the objective function for the new decision variables
  for (std::size_t i = A_original_ncol; i < A_feature_column_start; ++i)
    ptr->_obj.push_back(0.0);

  // add lb for new decision variables
  for (std::size_t i = A_original_ncol; i < A_feature_column_start; ++i)
    ptr->_lb.push_back(0.0);

  // add ub for new decision variables
  for (std::size_t i = A_original_ncol; i < A_feature_column_start; ++i)
    ptr->_ub.push_back(1.0);

  // add vtype for new decision variables
  for (std::size_t i = A_original_ncol; i < A_feature_column_start; ++i)
    ptr->_vtype.push_back(ptr->_vtype[0]);

  // add col ids for new decision variables
  for (std::size_t i = A_original_ncol; i < A_feature_column_start; ++i)
    ptr->_col_ids.push_back("c");

  // return result
  return true;
}
