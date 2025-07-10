#include "package.h"
#include "optimization_problem.h"
#include "functions.h"

// [[Rcpp::export]]
bool rcpp_apply_neighbor_constraints(
  SEXP x, const Rcpp::List connected_data, bool clamp,
  std::vector<std::size_t> k, std::size_t max_n
) {

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
  std::size_t n_dv = ptr->_number_of_planning_units * ptr->_number_of_zones;

  // convert the list of list of sparseMatrix objects to a Rcpp classes
  std::vector<std::vector<arma::sp_mat>> data;
  import_connectivity_matrix_list(connected_data, data, true);

  // calculate total number of non-zero elements in data
  std::size_t data_nonzero = 0;
  for (std::size_t z1 = 0; z1 < ptr->_number_of_zones; ++z1)
    for (std::size_t z2 = 0; z2 < ptr->_number_of_zones; ++z2)
      data_nonzero += data[z1][z2].n_nonzero;

  // declare variables
  std::vector<std::size_t> pu_i;
  pu_i.reserve(data_nonzero);
  std::vector<std::size_t> pu_j;
  pu_j.reserve(data_nonzero);
  std::size_t curr_i, curr_j;
  double curr_value;
  bool curr_is_feasible;
  // note that because these are sets, pu_neighbor_set[i] will only
  // contain unique indices for planning units that are neighbors for i
  std::vector<boost::unordered_set<std::size_t>> pu_neighbor_set(n_dv);
  for (std::size_t i = 0; i < n_dv; ++i) {
    pu_neighbor_set[i].reserve(max_n);
  }

  // extract data from the connected matrix
  for (std::size_t z1 = 0; z1 < ptr->_number_of_zones; ++z1) {
    for (std::size_t z2 = 0; z2 < ptr->_number_of_zones; ++z2) {
      for (arma::sp_mat::const_iterator itr = data[z1][z2].begin();
           itr != data[z1][z2].end(); ++itr) {
        // extract cell data
        curr_i = (z1 * ptr->_number_of_planning_units) + itr.row();
        curr_j = (z2 * ptr->_number_of_planning_units) + itr.col();
        curr_value = *itr;
        // determine if connection between curr_i and curr_j is feasible
        // AND or curr_i NOT locked in
        curr_is_feasible =
          (ptr->_ub[curr_i] > 1.0e-5) &&
          (ptr->_ub[curr_j] > 1.0e-5) &&
          (ptr->_lb[curr_i] < 0.5);
        // add connection to the map if...
        // (i) the number indicating cell connection is greater than a very
        // small positive number
        // (ii) the connection is not simply the same cell
        // (iii) NOT clamp OR connection is feasible
        if (
          (curr_value > 1.0e-15) &&
          (curr_i != curr_j) &&
          (!clamp || curr_is_feasible)
        ) {
          // store connection
          pu_i.push_back(curr_i);
          pu_j.push_back(curr_j);
          // store the neighboring planning unit
          /// note that we don't store curr_j here (i.e., the index
          /// for the decsion variable of pu_j_zone_z2), and instead the
          /// index of the planning unit (i.e., pu_j) so we can
          /// identify the maximum number of feasible neighbors given
          /// the locked constraints
          pu_neighbor_set[curr_i].insert(itr.col());
        }
      }
    }
  }

  // add constraints to specify that each planning unit should have
  // k number of neighbors
  double clamp2 = static_cast<double>(clamp);
  for (std::size_t z = 0; z < (ptr->_number_of_zones); ++z) {
    for (std::size_t i = 0; i < (ptr->_number_of_planning_units); ++i) {
      // identify decision variable
      curr_i = (z * ptr->_number_of_planning_units) + i;
      ptr->_A_i.push_back(A_original_nrow + curr_i);
      ptr->_A_j.push_back(curr_i);
      ptr->_A_x.push_back(
        // if clamp, then use min(...), otherwise use k[z]
        -1.0 * (
          (clamp2 * std::min(k[z], pu_neighbor_set[curr_i].size())) +
          ((1.0 - clamp2) * k[z])
        )
      );
      ptr->_sense.push_back(">=");
      ptr->_rhs.push_back(0.0);
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
