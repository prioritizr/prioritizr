#include "package.h"
#include "optimization_problem.h"
#include "rcpp_boundary_data.h"
#include "functions.h"

// [[Rcpp::export]]
bool rcpp_apply_asymmetric_connectivity_constraints(SEXP x,
  Rcpp::List connectivity_data, double penalty) {
  // initialization
  typedef std::pair<std::size_t,std::size_t> PUPAIRID;
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t A_original_ncol = ptr->_obj.size();
  std::size_t A_original_nrow = ptr->_rhs.size();

  // convert the list of list of sparseMatrix objects to a Rcpp classes
  std::vector<std::vector<arma::sp_mat>> data;
  import_connectivity_matrix_list(connectivity_data, data, true);

  // calculate total number of non-zero elements in data
  std::size_t data_nonzero = 0;
  for (std::size_t z1 = 0; z1 < ptr->_number_of_zones; ++z1)
    for (std::size_t z2 = z1; z2 < ptr->_number_of_zones; ++z2)
      data_nonzero += data[z1][z2].n_nonzero;

  // extract data from the scaled boundary matrices
  std::vector<double> total_penalties(ptr->_number_of_planning_units *
                                      ptr->_number_of_zones, 0.0);
  std::unordered_map<PUPAIRID, double, boost::hash<PUPAIRID>> connections;
  connections.reserve(data_nonzero);
  std::size_t curr_i, curr_j, curr_col1, curr_col2;
  double curr_value;
  PUPAIR curr_pu_pair;
  std::unordered_map<PUPAIRID, double, boost::hash<PUPAIRID>>::iterator curr_itr;
  arma::sp_mat curr_scaled_penalty_matrix;
  for (std::size_t z1 = 0; z1 < ptr->_number_of_zones; ++z1) {
    for (std::size_t z2 = 0; z2 < ptr->_number_of_zones; ++z2) {
      // scale the connectivity matrix
      curr_scaled_penalty_matrix = data[z1][z2];
      curr_scaled_penalty_matrix *= penalty;
      // extract elements
      for (arma::sp_mat::const_iterator it =
             curr_scaled_penalty_matrix.begin();
           it != curr_scaled_penalty_matrix.end(); ++it) {
        // get row and column indices for cell
        curr_i = it.row();
        curr_j = it.col();
        curr_value = *it;
        if (std::fabs(curr_value) > 1.0e-15) {
          if ((curr_i == curr_j) && (z1 == z2)) {
            // amount of exposed connection in planning unit with no neighbours
            curr_col1 = (z1 * ptr->_number_of_planning_units) + curr_i;
            total_penalties[curr_col1] += curr_value;
          } else {
            // amount of shared connection between two different
            // planning units in the same zone or in different zones
            // add pi_z1 connection
            curr_col1 = (z1 * ptr->_number_of_planning_units) + curr_i;
            total_penalties[curr_col1] += curr_value;
            // add pj_z1 connection
            curr_col2 = (z2 * ptr->_number_of_planning_units) + curr_j;
            // store connection
            curr_pu_pair = PUPAIR(curr_col1, curr_col2, curr_value);
            curr_itr = connections.find(curr_pu_pair._id);
            if (curr_itr == connections.end()) {
              connections.insert(std::pair<PUPAIRID, double>(curr_pu_pair._id,
                                                             curr_value));
            } else {
              (curr_itr->second) += curr_value;
            }
          }
        }
      }
    }
  }

  // if the objective is to minimize the costs, then boundary penalties are
  // positive
  if (ptr->_modelsense == "min") {
    // add total boundaries to planning unit costs in obj
    for (std::size_t i = 0;
         i < (ptr->_number_of_zones * ptr->_number_of_planning_units); ++i)
      ptr->_obj[i] += total_penalties[i];
    // add exposed boundaries to obj
    for (auto itr = connections.cbegin(); itr != connections.cend(); ++itr)
      ptr->_obj.push_back(itr->second);
  }

  // if the objective is to maximize the costs, then boundary penalties are
  // negative
  if (ptr->_modelsense == "max") {
    // add total boundaries to planning unit costs in obj
    for (std::size_t i = 0;
         i < (ptr->_number_of_zones * ptr->_number_of_planning_units); ++i)
      ptr->_obj[i] -= total_penalties[i];
    // add exposed boundaries to obj
    for (auto itr = connections.cbegin(); itr != connections.cend(); ++itr)
      ptr->_obj.push_back(itr->second);
  }

  // add lb for new decision variables
  for (auto itr = connections.cbegin(); itr != connections.cend(); ++itr)
    ptr->_lb.push_back(0.0);

  // add ub for new decision variables
  for (auto itr = connections.cbegin(); itr != connections.cend(); ++itr)
    ptr->_ub.push_back(1.0);

  // add vtype for new decision variables
  for (auto itr = connections.cbegin(); itr != connections.cend(); ++itr)
    ptr->_vtype.push_back(ptr->_vtype[0]);

  // add col ids for new decision variables
  for (auto itr = connections.cbegin(); itr != connections.cend(); ++itr)
    ptr->_col_ids.push_back("ac");

  // add new constraints to
  std::size_t i = -1;
  std::size_t A_row = (A_original_nrow - 1);
  for (auto itr = connections.cbegin(); itr != connections.cend(); ++itr) {
    // increment row
    ++A_row;
    // increment variable counter
    ++i;
    // constraint to ensure that decision variable pu_i_zone_a_pu_j_zone_b
    // is less than or equal to pu_i_zone_a
    ptr->_A_i.push_back(A_row);
    ptr->_A_i.push_back(A_row);
    ptr->_A_j.push_back(A_original_ncol + i);
    ptr->_A_j.push_back(itr->first.first);
    ptr->_A_x.push_back(1.0);
    ptr->_A_x.push_back(-1.0);
    ptr->_sense.push_back("<=");
    ptr->_rhs.push_back(0.0);
    ptr->_row_ids.push_back("ac1");

    // constraint to ensure that decision variable pu_i_zone_a_pu_j_zone_b is
    // less than or equal to pu_j_zone_b
    ++A_row;
    ptr->_A_i.push_back(A_row);
    ptr->_A_i.push_back(A_row);
    ptr->_A_j.push_back(A_original_ncol + i);
    ptr->_A_j.push_back(itr->first.second);
    ptr->_A_x.push_back(1.0);
    ptr->_A_x.push_back(-1.0);
    ptr->_sense.push_back("<=");
    ptr->_rhs.push_back(0.0);
    ptr->_row_ids.push_back("ac2");

    // constraint to ensure that decision variable pu_i_zone_a_pu_j_zone_b
    // is 1 iff pu_i_zone_a is 1 and pu_j_zone_b is 1
    // this is only needed when the penalties are negative
    if ((itr->second) < 0) {
      ++A_row;
      ptr->_A_i.push_back(A_row);
      ptr->_A_i.push_back(A_row);
      ptr->_A_i.push_back(A_row);
      ptr->_A_j.push_back(A_original_ncol + i);
      ptr->_A_j.push_back(itr->first.first);
      ptr->_A_j.push_back(itr->first.second);
      ptr->_A_x.push_back(1.0);
      ptr->_A_x.push_back(-1.0);
      ptr->_A_x.push_back(-1.0);
      ptr->_sense.push_back(">=");
      ptr->_rhs.push_back(-1.0);
      ptr->_row_ids.push_back("ac3");
    }
  }

  // return result
  return true;

}
