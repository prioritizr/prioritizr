#include "package.h"
#include "optimization_problem.h"
#include "functions.h"
#include <map>
#include <set>

// [[Rcpp::export]]
bool rcpp_apply_separation_constraints(
  SEXP x, const Rcpp::List connected_data
) {

  /* The following code makes the following critical assumptions
   *
   * 1. connected_data is a list of lists containing sparse matrices. Each
   *    matrix has the same number of rows and columns, and these are
   *    equal to the number of planning units in x. The nested list
   *    structure of the object is
   *    connected_data[[1:ptr->_number_of_zones]][[1:ptr->_number_of_zones]]
   *
   * 2. the problem already contains constraints so that the total
   *    allocation of each planning unit across all zones is at most one
   *    (see rcpp_add_zones_constraints). This means that, for a given
   *    planning unit j, at most one of the decision variables for j can be
   *    selected, so they can be summed in a single constraint.
   *
   * For each planning unit i allocated to zone z1, and each planning unit j
   * that is a neighbor of i when they are allocated to zones z1 and z2
   * (respectively), the following constraint is added:
   *
   *   x[i, z1] + sum_{z2} x[j, z2] <= 1
   *
   * So, if planning unit i is allocated to zone z1, then none of its
   * neighbors can be allocated to a zone z2 that is separated from z1.
   *
   */

  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t A_original_nrow = ptr->_rhs.size();
  std::size_t n_pu = ptr->_number_of_planning_units;
  std::size_t n_z = ptr->_number_of_zones;

  // convert the list of list of sparseMatrix objects to a Rcpp classes
  std::vector<std::vector<arma::sp_mat>> data;
  import_connectivity_matrix_list(connected_data, data, true);

  // map each (pu_i, zone_z1, pu_j) combination to the decision variables
  // for pu_j that cannot be selected if pu_i is allocated to zone_z1
  // note that std::map is used so that the constraints are added in a
  // deterministic order
  std::map<std::array<std::size_t, 3>, std::set<std::size_t>> conflicts;
  for (std::size_t z1 = 0; z1 < n_z; ++z1) {
    for (std::size_t z2 = 0; z2 < n_z; ++z2) {
      for (arma::sp_mat::const_iterator itr = data[z1][z2].begin();
           itr != data[z1][z2].end(); ++itr) {
        // skip if the cell does not indicate a connection, or the
        // connection is simply the same cell
        if ((*itr < 1.0e-15) || (itr.row() == itr.col()))
          continue;
        // store the decision variable for pu_j in zone z2
        conflicts[{itr.row(), z1, itr.col()}].insert(
          (z2 * n_pu) + itr.col()
        );
      }
    }
  }

  // build the constraints, skipping constraints that are duplicates of
  // constraints that have already been added (e.g., when there is a single
  // zone, the constraint for pu_i and pu_j is the same as that for pu_j and
  // pu_i)
  std::set<std::set<std::size_t>> rows;
  for (auto itr = conflicts.cbegin(); itr != conflicts.cend(); ++itr) {
    std::set<std::size_t> curr_row = itr->second;
    curr_row.insert((itr->first[1] * n_pu) + itr->first[0]);
    rows.insert(curr_row);
  }

  // add constraints
  std::size_t r = A_original_nrow;
  for (auto itr = rows.cbegin(); itr != rows.cend(); ++itr) {
    for (auto itr2 = itr->cbegin(); itr2 != itr->cend(); ++itr2) {
      ptr->_A_i.push_back(r);
      ptr->_A_j.push_back(*itr2);
      ptr->_A_x.push_back(1.0);
    }
    ptr->_sense.push_back("<=");
    ptr->_rhs.push_back(1.0);
    ptr->_row_ids.push_back("s");
    ++r;
  }

  // return result
  return true;
}
