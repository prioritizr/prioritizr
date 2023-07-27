#include "package.h"
#include "optimization_problem.h"
#include "functions.h"

// [[Rcpp::export]]
bool rcpp_apply_neighbor_constraints(
  SEXP x, const Rcpp::List connected_data, const Rcpp::NumericMatrix k) {

  /* The following code makes the following critical assumptions
   *
   * 1. connectivity_data is a list of lists containing sparse matrices. Each
   *    matrix has the same number of rows and columns, and these are
   *    equal to the number of planning units in x. The nested list
   *    structure of the object is
   *    connected_data[[1:ptr->_number_of_zones]][[1:ptr->_number_of_zones]]
   *
   * 2. k is a matrix containing a row for each different planning unit,
   *    and a column for each zone. Cell values indicate the number of
   *    neighbors that a planning unit must have when allocated to a particular
   *    zone.
   *
   */

  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t A_original_nrow = ptr->_rhs.size();

  // convert the list of list of sparseMatrix objects to a Rcpp classes
  std::vector<std::vector<arma::sp_mat>> data;
  import_connectivity_matrix_list(connected_data, data, true);

  // calculate total number of non-zero elements in data
  std::size_t data_nonzero = 0;
  for (std::size_t z1 = 0; z1 < ptr->_number_of_zones; ++z1)
    for (std::size_t z2 = 0; z2 < ptr->_number_of_zones; ++z2)
      data_nonzero += data[z1][z2].n_nonzero;

  // extract data from the connected matrix
  std::vector<std::size_t> pu_i;
  pu_i.reserve(data_nonzero);
  std::vector<std::size_t> pu_j;
  pu_j.reserve(data_nonzero);
  std::size_t curr_i, curr_j;
  double curr_value;
  for (std::size_t z1 = 0; z1 < ptr->_number_of_zones; ++z1) {
    for (std::size_t z2 = 0; z2 < ptr->_number_of_zones; ++z2) {
      for (arma::sp_mat::const_iterator itr = data[z1][z2].begin();
           itr != data[z1][z2].end(); ++itr) {
        // extract cell data
        curr_i = itr.row();
        curr_j = itr.col();
        curr_value = *itr;
        // if the number indicating cell connection is greater than a very
        // small positive number then include it
        if ((curr_value > 1.0e-15) && (curr_i != curr_j)) {
          pu_i.push_back((z1 * ptr->_number_of_planning_units) + curr_i);
          pu_j.push_back((z2 * ptr->_number_of_planning_units) + curr_j);
        }
      }
    }
  }

  // add constraints to specify that each planning unit should have
  // k number of neighbors
  for (std::size_t z = 0; z < (ptr->_number_of_zones); ++z) {
    for (std::size_t i = 0; i < (ptr->_number_of_planning_units); ++i) {
      ptr->_A_i.push_back(A_original_nrow +
                          (z * ptr->_number_of_planning_units) + i);
      ptr->_A_j.push_back((z * ptr->_number_of_planning_units) + i);
      ptr->_A_x.push_back(k(i, z) * -1.0);
      ptr->_sense.push_back(">=");
      ptr->_rhs.push_back(0);
      ptr->_row_ids.push_back("n");
    }
  }

  // add constraints to specify the connected relationships
  // constraint to ensure that pu_i is connected to pu_j
  for (auto itr = pu_i.cbegin(); itr != pu_i.cend(); ++itr)
    ptr->_A_i.push_back(A_original_nrow + (*itr));
  for (auto itr = pu_j.cbegin(); itr != pu_j.cend(); ++itr)
    ptr->_A_j.push_back(*itr);
  for (auto itr = pu_j.cbegin(); itr != pu_j.cend(); ++itr)
    ptr->_A_x.push_back(1.0);

  // return result
  return true;
}
