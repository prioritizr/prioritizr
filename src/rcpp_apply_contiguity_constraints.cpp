#include "package.h"
#include "optimization_problem.h"
#include "functions.h"

// [[Rcpp::export]]
bool rcpp_apply_contiguity_constraints(
  SEXP x, const arma::sp_mat data, const Rcpp::IntegerVector clusters) {

  /* The following code makes the following critical assumptions
   *
   * 1. data is a sparse matrix where each row and column corresponds to
   *    a different planning unit.
   *
   * 2. clusters is an integer vector where each element corresponds to
   *    a different zone. Zones with the same number should be
   *    treated as being able to be connected to each other. Zones that
   *    are not subject to any contiguity constraints are represented with
   *    a zero.
   */

  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t A_original_ncol = ptr->_obj.size();
  std::size_t A_original_nrow = ptr->_rhs.size();

  // count number of total clusters
  const size_t n_clusters = Rcpp::max(clusters);

  // store zone indices for each cluster
  std::vector<std::vector<size_t>> cluster_ids(n_clusters);
  for (std::size_t c = 0; c < n_clusters; ++c)
    cluster_ids[c].reserve(ptr->_number_of_zones);
  for (std::size_t z = 0; z < ptr->_number_of_zones; ++z)
    if (clusters[z] > 0)
      cluster_ids[clusters[z] - 1].push_back(z);

  // allocate vectors to hold data
  std::size_t data_nonzero;
  std::vector<std::vector<std::size_t>> pu_i(n_clusters);
  std::vector<std::vector<std::size_t>> pu_j(n_clusters);
  for (std::size_t c = 0; c < n_clusters; ++c) {
    // find out how many connections associated with the cluster
    data_nonzero = 0;
    for (std::size_t z = 0; z < cluster_ids[c].size(); ++z)
        data_nonzero += data.n_nonzero;
    // preallocate data for the zone
    pu_i[c].reserve(data_nonzero);
    pu_j[c].reserve(data_nonzero);
  }

  // extract data
  std::size_t curr_i, curr_j;
  double curr_value;
  for (std::size_t c = 0; c < n_clusters; ++c) {
    for (std::size_t z1 = 0; z1 < cluster_ids[c].size(); ++z1) {
      for (std::size_t z2 = z1; z2 < cluster_ids[c].size(); ++z2) {
        for (arma::sp_mat::const_iterator itr = data.begin();
             itr != data.end(); ++itr) {
          // extract cell data
          curr_i = itr.row();
          curr_j = itr.col();
          curr_value = *itr;
          // if the number indicating cell connection is greater than a very
          // small positive number then include it
          if ((curr_value > 1.0e-15) && (curr_i != curr_j)) {
            pu_i[c].push_back((cluster_ids[c][z1] *
                               ptr->_number_of_planning_units) + curr_i);
            pu_j[c].push_back((cluster_ids[c][z2] *
                               ptr->_number_of_planning_units) + curr_j);
          }
        }
      }
    }
  }

  // sort data in pu_i and pu_j
  std::vector<std::size_t> order;
  std::vector<std::size_t> tmp;
  if (ptr->_number_of_zones > 1) {
    for (std::size_t c = 0; c < n_clusters; ++c) {
      // initialize vector
      order.resize(pu_i[c].size());
      std::iota(order.begin(), order.end(), 0);
      // sort order by values in pu_i and pu_j
      std::sort(order.begin(), order.end(), [&](int a, int b) {
        if (pu_j[c][a] != pu_j[c][b])
          return pu_j[c][a] < pu_j[c][b];
        return pu_i[c][a] < pu_i[c][b];
      });
      // reorder values in pu_i and pu_j
      tmp = pu_i[c];
      for (std::size_t i = 0; i < tmp.size(); ++i)
       pu_i[c][i] = tmp[order[i]];
      tmp = pu_j[c];
      for (std::size_t i = 0; i < tmp.size(); ++i)
       pu_j[c][i] = tmp[order[i]];
    }
  }

  // calcuate the starting index for the variables representing each zone
  std::vector<std::size_t> connection_variable_indices(n_clusters);
  connection_variable_indices[0] = 0;
  for (std::size_t c = 1; c < n_clusters; ++c)
    connection_variable_indices[c] = connection_variable_indices[c - 1] +
                                     pu_i[c - 1].size();

  // add in zeros to the objective function for the new decision variables
  for (std::size_t c = 0; c < n_clusters; ++c)
    for (auto i = pu_i[c].cbegin(); i != pu_i[c].cend(); ++i)
      ptr->_obj.push_back(0.0);

  // add lb for new decision variables
  for (std::size_t c = 0; c < n_clusters; ++c)
    for (auto i = pu_i[c].cbegin(); i != pu_i[c].cend(); ++i)
      ptr->_lb.push_back(0.0);

  // add ub for new decision variables
  for (std::size_t c = 0; c < n_clusters; ++c)
    for (auto i = pu_i[c].cbegin(); i != pu_i[c].cend(); ++i)
      ptr->_ub.push_back(1.0);

  // add vtype for new decision variables
  for (std::size_t c = 0; c < n_clusters; ++c)
    for (auto i = pu_i[c].cbegin(); i != pu_i[c].cend(); ++i)
      ptr->_vtype.push_back(ptr->_vtype[0]);

  // add col ids for new decision variables
  for (std::size_t c = 0; c < n_clusters; ++c)
    for (auto i = pu_i[c].cbegin(); i != pu_i[c].cend(); ++i)
      ptr->_col_ids.push_back("c");

  // add new constraints to ensure selected planning units are connected
  std::size_t A_row = (A_original_nrow - 1);

  // constraint to ensure that pu_ij <= pu_i, i.e. the variable representing
  // the connection between two planning units is only 1 when both of
  // the planning units are 1
  for (std::size_t c = 0; c < n_clusters; ++c) {
    for (std::size_t i = 0; i < (pu_i[c].size()); ++i) {
      // increment row
      ++A_row;
      // add constraint
      ptr->_A_i.push_back(A_row);
      ptr->_A_i.push_back(A_row);
      ptr->_A_j.push_back(A_original_ncol + connection_variable_indices[c] + i);
      ptr->_A_j.push_back(pu_i[c][i]);
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
  for (std::size_t c = 0; c < n_clusters; ++c) {
    for (std::size_t i = 0; i < (pu_i[c].size()); ++i) {
      if (pu_j[c][i] != current_pu_j) {
        ++A_row;
        current_pu_j = pu_j[c][i];
        ptr->_A_i.push_back(A_row);
        ptr->_A_j.push_back(current_pu_j);
        ptr->_A_x.push_back(-1.0);
        ptr->_sense.push_back("<=");
        ptr->_row_ids.push_back("c2");
        ptr->_rhs.push_back(0.0);
      }
      ptr->_A_i.push_back(A_row);
      ptr->_A_j.push_back(A_original_ncol + connection_variable_indices[c] + i);
      ptr->_A_x.push_back(1.0);
    }
  }

  // add constraints to ensure that the number of active connections between
  // planning units is equal to the number of selected planning units minus one.
  // this constraint ensures that the solution is actually connected
  for (std::size_t c = 0; c < n_clusters; ++c) {
    ++A_row;
    for (std::size_t z = 0; z < cluster_ids[c].size(); ++z) {
      for (std::size_t i = 0; i < ptr->_number_of_planning_units; ++i) {
        // add planning unit values
        ptr->_A_i.push_back(A_row);
        ptr->_A_j.push_back((cluster_ids[c][z] *
                            (ptr->_number_of_planning_units)) + i);
        ptr->_A_x.push_back(-1.0);
      }
    }
    for (std::size_t i = 0; i < (pu_i[c].size()); ++i) {
      // add connection values
      ptr->_A_i.push_back(A_row);
      ptr->_A_j.push_back(A_original_ncol + connection_variable_indices[c] + i);
      ptr->_A_x.push_back(1.0);
    }
    ptr->_sense.push_back("=");
    ptr->_rhs.push_back(-1);
    ptr->_row_ids.push_back("c3");
  }

  // return result
  return true;
}
