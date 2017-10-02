#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_add_rij_data(SEXP x, arma::sp_mat rij, bool compressed_formulation) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  // declare variables
  std::size_t row;
  std::size_t col;
  // assign indices
  ptr->_number_of_features = static_cast<std::size_t>(rij.n_rows);
  ptr->_number_of_planning_units = static_cast<std::size_t>(rij.n_cols);
  ptr->_planning_unit_indices.resize(ptr->_number_of_planning_units);
  std::iota(ptr->_planning_unit_indices.begin(),
            ptr->_planning_unit_indices.end(), 0);
  ptr->_compressed_formulation = compressed_formulation;
  // add rij data and preliminary constraints
  if (compressed_formulation) {
    // assign decision matrix variables
    for (arma::sp_mat::const_iterator it=rij.begin(); it!=rij.end(); ++it) {
      ptr->_A_i.push_back(it.row());
      ptr->_A_j.push_back(it.col());
      ptr->_A_x.push_back(*it);
    }
    for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
      ptr->_col_ids.push_back("pu");
  } else {
    // define initial row value;
    row = -1;
    // add in constraints that determine if each planning unit is used to
    // represent each feature
    for (std::size_t f = 0; f < (ptr->_number_of_features); ++f) {
      for (std::size_t j = 0; j < (ptr->_number_of_planning_units); ++j) {
        ++row;
        // add constrains to ensure that y_ij <= x_j
        ptr->_A_i.push_back(row);
        ptr->_A_i.push_back(row);
        ptr->_A_j.push_back(j);
        ptr->_A_j.push_back(ptr->_number_of_planning_units +
                            (f * ptr->_number_of_planning_units) + j);
        ptr->_A_x.push_back(-1.0);
        ptr->_A_x.push_back(1.0);
      }
    }
    // add in rij data
    ++row;
    for (arma::sp_mat::const_iterator itr = rij.begin(); itr != rij.end();
         ++itr) {
      // add rij_data for y_ij
      ptr->_A_i.push_back(row + itr.row());
      ptr->_A_j.push_back(ptr->_number_of_planning_units +
                          (itr.row() * ptr->_number_of_planning_units) +
                          itr.col());
      ptr->_A_x.push_back(*itr);
    }
    // add in constraint information
    for (std::size_t i = 0; i < row; ++i)
      ptr->_sense.push_back("<=");
    for (std::size_t i = 0; i < row; ++i)
      ptr->_rhs.push_back(0.0);
    for (std::size_t i = 0; i < row; ++i)
      ptr->_row_ids.push_back("pu_ij");
    for (std::size_t i = 0; i < rij.n_cols; ++i)
      ptr->_col_ids.push_back("pu");
    for (std::size_t i = 0; i < (rij.n_cols * rij.n_rows); ++i)
      ptr->_col_ids.push_back("pu_ij");
  }
  // return result
  return true;
}
