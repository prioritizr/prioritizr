#include "package.h"
#include "optimization_problem.h"
#include "functions.h"

// [[Rcpp::export]]
bool rcpp_apply_connected_constraints(SEXP x, Rcpp::List connected_data,
                                      Rcpp::LogicalVector y) {

  /* The following code makes the following critical assumptions
   *
   * 1. connectivity_data is a list of lists containing sparse matrices. Each
   *    matrix has the same number of rows and columns, and these are
   *    equal to the number of planning units in x. The nested list
   *    structure of the object is
   *    connected_data[[1:ptr->_number_of_zones]][[1:ptr->_number_of_zones]]
   *
   * 2. the number of elements in k is equal to ptr->_number_of_zones
   *
   */

  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t A_original_ncol = ptr->_obj.size();
  std::size_t A_original_nrow = ptr->_rhs.size();

  std::vector<size_t> y_indices;
  y_indices.reserve(ptr->_number_of_zones);
  for (std::size_t z = 0; z < ptr->_number_of_zones; ++z)
    if (y[z])
      y_indices.push_back(z);
  std::size_t n_constrainted_zones = y_indices.size();

  // convert the list of list of sparseMatrix objects to a Rcpp classes
  std::vector<std::vector<arma::sp_mat>> data;
  import_connectivity_matrix_list(connected_data, data, false);

  // allocate vectors to hold data
  std::size_t data_nonzero;
  std::vector<std::vector<std::size_t>> pu_i(ptr->_number_of_zones);
  std::vector<std::vector<std::size_t>> pu_j(ptr->_number_of_zones);
  for (std::size_t z1 = 0; z1 < n_constrainted_zones; ++z1) {
    // find out how many connections associated with the zone
    data_nonzero = 0;
    for (std::size_t z2 = 0; z2 < ptr->_number_of_zones; ++z2)
      data_nonzero += data[y_indices[z1]][z2].n_nonzero;
    // preallocate data for the zone
    pu_i[z1].reserve(data_nonzero);
    pu_j[z1].reserve(data_nonzero);
  }

  // extract data
  std::size_t curr_i, curr_j;
  double curr_value;
  for (std::size_t z1 = 0; z1 < n_constrainted_zones; ++z1) {
    for (std::size_t z2 = 0; z2 < ptr->_number_of_zones; ++z2) {
      for (arma::sp_mat::const_iterator itr = data[y_indices[z1]][z2].begin();
           itr != data[y_indices[z1]][z2].end(); ++itr) {
        // extract cell data
        curr_i = itr.row();
        curr_j = itr.col();
        curr_value = *itr;
        // if the number indicating cell connection is greater than a very
        // small positive number then include it
        if ((curr_value > 1.0e-15) && (curr_i != curr_j)) {
          pu_i[z1].push_back((z1 * ptr->_number_of_planning_units) + curr_i);
          pu_j[z1].push_back((z2 * ptr->_number_of_planning_units) + curr_j);
        }
      }
    }
  }

  // calcuate the starting index for the variables representing each zone
  std::vector<std::size_t> connection_variable_indices(n_constrainted_zones);
  connection_variable_indices[0] = 0;
  for (std::size_t z = 1; z < n_constrainted_zones; ++z)
    connection_variable_indices[z] = connection_variable_indices[z - 1] +
                                     pu_i[z - 1].size();

  // add in zeros to the objective function for the new decision variables
  for (std::size_t z = 0; z < n_constrainted_zones; ++z)
    for (auto i = pu_i[z].cbegin(); i != pu_i[z].cend(); ++i)
      ptr->_obj.push_back(0.0);

  // add lb for new decision variables
  for (std::size_t z = 0; z < n_constrainted_zones; ++z)
    for (auto i = pu_i[z].cbegin(); i != pu_i[z].cend(); ++i)
      ptr->_lb.push_back(0.0);

  // add ub for new decision variables
  for (std::size_t z = 0; z < n_constrainted_zones; ++z)
    for (auto i = pu_i[z].cbegin(); i != pu_i[z].cend(); ++i)
      ptr->_ub.push_back(1.0);

  // add vtype for new decision variables
  for (std::size_t z = 0; z < n_constrainted_zones; ++z)
    for (auto i = pu_i[z].cbegin(); i != pu_i[z].cend(); ++i)
      ptr->_vtype.push_back(ptr->_vtype[0]);

  // add col ids for new decision variables
  for (std::size_t z = 0; z < n_constrainted_zones; ++z)
    for (auto i = pu_i[z].cbegin(); i != pu_i[z].cend(); ++i)
      ptr->_col_ids.push_back("c");

  // add new constraints to ensure selected planning units are connected
  std::size_t A_row = (A_original_nrow - 1);

  // constraint to ensure that pu_ij <= pu_i, i.e. the variable representing
  // the connection between two planning units is only 1 when both of
  // the planning units are 1
  for (std::size_t z = 0; z < n_constrainted_zones; ++z) {
    for (std::size_t i = 0; i < (pu_i[z].size()); ++i) {
      // increment row
      ++A_row;
      // add constraint
      ptr->_A_i.push_back(A_row);
      ptr->_A_i.push_back(A_row);
      ptr->_A_j.push_back(A_original_ncol + connection_variable_indices[z] + i);
      ptr->_A_j.push_back(pu_i[z][i]);
      ptr->_A_x.push_back(1.0);
      ptr->_A_x.push_back(-1.0);
      ptr->_sense.push_back("<=");
      ptr->_rhs.push_back(0.0);
      ptr->_row_ids.push_back("c1");
    }
  }

  // initialize to a planning unit index that should not ever exist
  std::size_t current_pu_j = (ptr->_number_of_zones *
                              ptr->_number_of_planning_units) + 10;

  // constraint to ensure that \sum_{i \in neighbors of j} pu_ij <= pu_j
  // this ensure that if multiple connections are going to pu_j that
  // only one connection is active
  for (std::size_t z = 0; z < n_constrainted_zones; ++z) {
    for (std::size_t i = 0; i < (pu_i[z].size()); ++i) {
      if (pu_j[z][i] != current_pu_j) {
        ++A_row;
        current_pu_j = pu_j[z][i];
        ptr->_A_i.push_back(A_row);
        ptr->_A_j.push_back(current_pu_j);
        ptr->_A_x.push_back(-1.0);
        ptr->_sense.push_back("<=");
        ptr->_row_ids.push_back("c2");
        ptr->_rhs.push_back(0.0);
      }
      ptr->_A_i.push_back(A_row);
      ptr->_A_j.push_back(A_original_ncol + connection_variable_indices[z] + i);
      ptr->_A_x.push_back(1.0);
    }
  }

  // add constraints to ensure that the number of active connections between
  // planning units is equal to the number of selected planning units minus one.
  // this constraint ensures that the solution is actually connected
  for (std::size_t z = 0; z < n_constrainted_zones; ++z) {
    ++A_row;
    for (std::size_t i = 0; i < (ptr->_number_of_planning_units); ++i) {
      // add planning unit values
      ptr->_A_i.push_back(A_row);
      ptr->_A_j.push_back((y_indices[z] * (ptr->_number_of_planning_units)) +
                          i);
      ptr->_A_x.push_back(-1.0);
    }
    for (std::size_t i = 0; i < (pu_i[z].size()); ++i) {
      // add connection values
      ptr->_A_i.push_back(A_row);
      ptr->_A_j.push_back(A_original_ncol + connection_variable_indices[z] + i);
      ptr->_A_x.push_back(1.0);
    }
    ptr->_sense.push_back("=");
    ptr->_rhs.push_back(-1);
    ptr->_row_ids.push_back("c3");
  }

  // return result
  return true;
}
