#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_feature_contiguity_constraints(
  SEXP x, const Rcpp::List data, const Rcpp::List clusters_list) {

  /* The following code makes the following critical assumptions
  *
  * 1. data is a list of sparse matrix where each element corresponds to
  *    a different target in x, and each row and column in the matrices
  *    correspond to a different planning unit.
  *
  * 2. clusters_list is a list of integer vector where each element corresponds
  *    to a different feature and each element in the vector corresponds
  *    to a different zone. Zones with the same number should be
  *    treated as being able to be connected to each other. Zones that
  *    are not subject to any contiguity constraints are represented with
  *    a zero.
  *
  */

  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t A_original_ncol = ptr->_obj.size();
  std::size_t A_original_nrow = ptr->_rhs.size();

  // check that expanded formulation of the base conservation problem is used
  if (ptr->_compressed_formulation) {
    // # nocov start
    Rcpp::stop("this constraint requires the expanded formulation.");
    // # nocov end
  }

  // import cluster data
  std::vector<Rcpp::IntegerVector> clusters(ptr->_number_of_features);
  for (std::size_t f = 0; f < (ptr->_number_of_features); ++f)
    clusters[f] = Rcpp::as<Rcpp::IntegerVector>(clusters_list[f]);

  // import sparse matrices
  std::vector<arma::sp_mat> matrices(ptr->_number_of_features);
  for (std::size_t f = 0; f < (ptr->_number_of_features); ++f)
    matrices[f] = Rcpp::as<arma::sp_mat>(data[f]);

  // count number of total clusters for each feature
  std::vector<size_t> n_clusters_per_feature(ptr->_number_of_features);
  for (std::size_t f = 0; f < (ptr->_number_of_features); ++f)
    n_clusters_per_feature[f] = Rcpp::max(clusters[f]);
  std::size_t n_clusters = std::accumulate(n_clusters_per_feature.begin(),
                                           n_clusters_per_feature.end(), 0);

  // store zone indices for each cluster
  std::vector<std::vector<size_t>> cluster_zone_ids(n_clusters);
  std::vector<size_t> cluster_feature_ids(n_clusters);
  for (std::size_t c = 0; c < n_clusters; ++c)
    cluster_zone_ids[c].reserve(ptr->_number_of_zones);
  std::size_t c;
  for (std::size_t f = 0; f < (ptr->_number_of_features); ++f) {
    for (std::size_t z = 0; z < ptr->_number_of_zones; ++z) {
      if (clusters[f][z] > 0) {
        c = std::accumulate(n_clusters_per_feature.begin(),
                            n_clusters_per_feature.begin() + f, 0);
        c += (clusters[f][z] - 1);
        cluster_zone_ids[c].push_back(z);
        cluster_feature_ids[c] = f;
      }
    }
  }

  // allocate vectors to hold data
  std::size_t data_nonzero;
  std::vector<std::vector<std::size_t>> pu_i(n_clusters);
  std::vector<std::vector<std::size_t>> pu_j(n_clusters);
  for (std::size_t c = 0; c < n_clusters; ++c) {
    data_nonzero = 0;
    for (std::size_t z = 0; z < cluster_zone_ids[c].size(); ++z)
      data_nonzero += matrices[cluster_feature_ids[c]].n_nonzero;
    // preallocate data for the zone
    pu_i[c].reserve(data_nonzero);
    pu_j[c].reserve(data_nonzero);
  }

  // declare variables
  std::size_t A_row = (A_original_nrow - 1);

  // extract data
  std::size_t curr_i, curr_j, curr_f;
  double curr_value;
  for (std::size_t c = 0; c < n_clusters; ++c) {
    curr_f = cluster_feature_ids[c];
    for (std::size_t z1 = 0; z1 < cluster_zone_ids[c].size(); ++z1) {
      for (std::size_t z2 = z1; z2 < cluster_zone_ids[c].size(); ++z2) {
        for (arma::sp_mat::const_iterator itr = matrices[curr_f].begin();
             itr != matrices[curr_f].end(); ++itr) {
            // extract cell data
            curr_i = itr.row();
            curr_j = itr.col();
            curr_value = *itr;
          // if the number indicating cell connection is greater than a very
          // small positive number then include it
          if ((curr_value > 1.0e-15) && (curr_i != curr_j)) {
            pu_i[c].push_back(
              (ptr->_number_of_planning_units * ptr->_number_of_zones) +
              (cluster_zone_ids[c][z1] * ptr->_number_of_planning_units *
               ptr->_number_of_features) +
              (curr_f * ptr->_number_of_planning_units) + curr_i);
            pu_j[c].push_back(
              (ptr->_number_of_planning_units * ptr->_number_of_zones) +
              (cluster_zone_ids[c][z2] * ptr->_number_of_planning_units *
               ptr->_number_of_features) +
              (curr_f * ptr->_number_of_planning_units) + curr_j);
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

  // calcuate the starting index for the variables representing each cluster
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
      ptr->_col_ids.push_back("fc");

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
      ptr->_row_ids.push_back("fc1");
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
        ptr->_row_ids.push_back("fc2");
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
    for (std::size_t z = 0; z < cluster_zone_ids[c].size(); ++z) {
      for (std::size_t i = 0; i < ptr->_number_of_planning_units; ++i) {
        // add planning unit values
        ptr->_A_i.push_back(A_row);
        ptr->_A_j.push_back(
          (ptr->_number_of_planning_units * ptr->_number_of_zones) +
          (cluster_zone_ids[c][z] * ptr->_number_of_planning_units *
           ptr->_number_of_features) +
          (cluster_feature_ids[c] * ptr->_number_of_planning_units) + i);
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
    ptr->_row_ids.push_back("fc3");
  }

  // return result
  return true;
}
