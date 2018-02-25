#include "package.h"
#include "functions.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_add_rij_data(SEXP x, Rcpp::List rij_list, Rcpp::List targets_list,
   bool compressed_formulation) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  // declare variables
  std::size_t row;
  std::size_t col;
  Rcpp::IntegerVector curr_z;
  Rcpp::IntegerVector targets_feature =  targets_list["feature"];
  targets_feature = targets_feature - 1;
  Rcpp::NumericVector targets_value = targets_list["value"];
  Rcpp::List targets_zone = targets_list["feature"];
  // import list of sparse matrices as a vector of sparse matrices
  // note that the matrices are transposed since armadillo doesn't support
  // row-major sparse matrices to make populating the problem object
  // more efficient
  std::vector<arma::sp_mat> rij;
  import_rij(rij_list, rij);
  // assign indices
  ptr->_number_of_features = static_cast<std::size_t>(rij[0].n_rows);
  ptr->_number_of_planning_units = static_cast<std::size_t>(rij[0].n_cols);
  ptr->_number_of_zones = static_cast<std::size_t>(rij_list.size());
  ptr->_compressed_formulation = compressed_formulation;
  // set up problem with rij data
  if (compressed_formulation) {
    // assign rij matrix variables
    for (std::size_t i = 0; i < targets_feature.size(); ++i) {
      curr_z = Rcpp::as<Rcpp::IntegerVector>(targets_zone[i]);
      curr_z = curr_z - 1;
      for (std::size_t z = 0; z < curr_z.size(); ++z) {
        for (auto it = rij[curr_z[z]].begin_col(targets_feature[i]);
             it != rij[curr_z[z]].end_col(targets_feature[i]); ++it) {
          ptr->_A_i.push_back((curr_z[z] * ptr->_number_of_planning_units) +
                              it.row());
          ptr->_A_j.push_back(i);
          ptr->_A_x.push_back(*it);
        }
      }
    }
    // assign decision matrix variables
    for (std::size_t z = 0; z < (ptr->_number_of_zones); ++z)
      for (std::size_t i = 0; i < (ptr->_number_of_planning_units); ++i)
        ptr->_col_ids.push_back("pu");
  } else {
    // define initial row value;
    row = -1;
    // add in constraints that determine if each planning unit is used to
    // represent each feature
    for (std::size_t i = 0; i < targets_feature.size(); ++i) {
      curr_z = Rcpp::as<Rcpp::IntegerVector>(targets_zone[i]);
      curr_z = curr_z - 1;
      for (std::size_t z = 0; z < curr_z.size(); ++z) {
        for (std::size_t j = 0; j < (ptr->_number_of_planning_units); ++j) {
          // add constrains to ensure that y_ij <= x_j
          ++row;
          ptr->_A_i.push_back(row);
          ptr->_A_i.push_back(row);
          ptr->_A_j.push_back(j);
          ptr->_A_j.push_back((ptr->_number_of_planning_units *
                               ptr->_number_of_zones) +
                              (curr_z[z] * ptr->_number_of_features *
                               ptr->_number_of_planning_units) +
                              (targets_feature[i] *
                               ptr->_number_of_planning_units) +
                              j);
          ptr->_A_x.push_back(-1.0);
          ptr->_A_x.push_back(1.0);
        }
      }
    }
    // add in rij data
    ++row;
    for (std::size_t i = 0; i < targets_feature.size(); ++i) {
      curr_z = Rcpp::as<Rcpp::IntegerVector>(targets_zone[i]);
      curr_z = curr_z - 1;
      for (std::size_t z = 0; z < curr_z.size(); ++z) {
        for (auto it = rij[curr_z[z]].begin_col(targets_feature[i]);
             it != rij[curr_z[z]].end_col(targets_feature[i]); ++it) {
          // add rij data for y_ij
          ptr->_A_i.push_back(row + i);
          ptr->_A_j.push_back((ptr->_number_of_planning_units *
                               ptr->_number_of_zones) +
                              (curr_z[z] * ptr->_number_of_features *
                               ptr->_number_of_planning_units) +
                              (it.col() * ptr->_number_of_planning_units) +
                              it.row());
          ptr->_A_x.push_back(*it);
        }
      }
    }
    // add in constraint information
    for (std::size_t i = 0; i < row; ++i)
      ptr->_sense.push_back("<=");
    for (std::size_t i = 0; i < row; ++i)
      ptr->_rhs.push_back(0.0);
    for (std::size_t i = 0; i < row; ++i)
      ptr->_row_ids.push_back("pu_ij");
    for (std::size_t i = 0;
         i < (ptr->_number_of_zones * ptr->_number_of_planning_units); ++i)
      ptr->_col_ids.push_back("pu");
    for (std::size_t i = 0;
         i < (ptr->_number_of_zones * ptr->_number_of_planning_units *
              ptr->_number_of_features); ++i)
      ptr->_col_ids.push_back("pu_ij");
  }
  // return result
  return true;
}
