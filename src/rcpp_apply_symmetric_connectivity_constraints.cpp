#include "package.h"
#include "optimization_problem.h"
#include "functions.h"

// [[Rcpp::export]]
bool rcpp_apply_symmetric_connectivity_constraints(SEXP x,
  Rcpp::List connectivity_data, double penalty) {

  /* The following code makes the following critical assumptions
   *
   * 1. connectivity_data is a list of lists containing sparse matrix with only
   *    cells in the upper triangle with the diagonal filled in.If this
   *    condition is not met, then the constraints will be incorrect.
   *
   * 2. the number of rows and columns in penalty is equal to
   *    ptr->_number_of_zones
   *
   * 4. the number of elements in edge_factor is equal to ptr->_number_of_zones
   */
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t A_original_ncol = ptr->_obj.size();
  std::size_t A_original_nrow = ptr->_rhs.size();

  // convert the list of list of sparseMatrix objects to a Rcpp classes
  std::vector<std::vector<arma::sp_mat>> data;
  import_connectivity_matrix_list(connectivity_data, data, false);

  // calculate total number of non-zero elements in data
  std::size_t data_nonzero = 0;
  for (std::size_t z1 = 0; z1 < ptr->_number_of_zones; ++z1)
    for (std::size_t z2 = z1; z2 < ptr->_number_of_zones; ++z2)
      data_nonzero += data[z1][z2].n_nonzero;

  // extract data from the scaled boundary matrices
  std::vector<double> total_penalties(ptr->_number_of_planning_units *
                                      ptr->_number_of_zones, 0.0);
  std::vector<std::size_t> pu_i;
  pu_i.reserve(data_nonzero);
  std::vector<std::size_t> pu_j;
  pu_j.reserve(data_nonzero);
  std::vector<double> pu_b;
  pu_b.reserve(data_nonzero);
  std::size_t curr_i, curr_j, curr_col1, curr_col2;
  double curr_value;
  arma::sp_mat curr_scaled_penalty_matrix;
  for (std::size_t z1 = 0; z1 < ptr->_number_of_zones; ++z1) {
    for (std::size_t z2 = z1; z2 < ptr->_number_of_zones; ++z2) {
      // scale the boundary matrix
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
        if (curr_value > 1.0e-15) {
          if ((curr_i == curr_j) && (z1 == z2)) {
            // amount of exposed boundary in planning unit with no neighbours
            curr_col1 = (z1 * ptr->_number_of_planning_units) + curr_i;
            total_penalties[curr_col1] += curr_value;
          } else if (z1 == z2) {
            // amount of shared boundary between two different planning units
            // in the same zone
            // add pi_z1 boundary
            curr_col1 = (z1 * ptr->_number_of_planning_units) + curr_i;
            total_penalties[curr_col1] += curr_value;
            // add pj_z1 boundary
            curr_col2 = (z1 * ptr->_number_of_planning_units) + curr_j;
            total_penalties[curr_col2] += curr_value;
            // store variable representing pi_z1_pj_z1
            pu_i.push_back(curr_col1);
            pu_j.push_back(curr_col2);
            pu_b.push_back(curr_value);
          } else {
            // amount of shared boundary between two different planning units
            // in two different zones
            // pi_z1 boundary
            curr_col1 = (z1 * ptr->_number_of_planning_units) + curr_i;
            total_penalties[curr_col1] += curr_value;
            // pj_z2 boundary
            curr_col2 = (z2 * ptr->_number_of_planning_units) + curr_j;
            total_penalties[curr_col2] += curr_value;
            // store variable representing pi_z1_pj_z2
            pu_i.push_back(curr_col1);
            pu_j.push_back(curr_col2);
            pu_b.push_back(curr_value);
            // pi_z2 boundary
            curr_col1 = (z2 * ptr->_number_of_planning_units) + curr_i;
            total_penalties[curr_col1] += curr_value;
            // pj_z1 boundary
            curr_col2 = (z1 * ptr->_number_of_planning_units) + curr_j;
            total_penalties[curr_col2] += curr_value;
            // store variable representing pi_z2_pj_z1
            pu_i.push_back(curr_col1);
            pu_j.push_back(curr_col2);
            pu_b.push_back(curr_value);
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
    for (auto i = pu_b.cbegin(); i != pu_b.cend(); ++i)
      ptr->_obj.push_back((*i) * -2.0);
  }

  // if the objective is to maximize the costs, then boundary penalties are
  // negative
  if (ptr->_modelsense == "max") {
    // add total boundaries to planning unit costs in obj
    for (std::size_t i = 0;
         i < (ptr->_number_of_zones * ptr->_number_of_planning_units); ++i)
      ptr->_obj[i] -= total_penalties[i];
    // add exposed boundaries to obj
    for (auto i = pu_b.cbegin(); i != pu_b.cend(); ++i)
      ptr->_obj.push_back((*i) * 2.0);
  }

  // add lb for new decision variables
  for (auto i = pu_i.cbegin(); i != pu_i.cend(); ++i)
    ptr->_lb.push_back(0.0);

  // add ub for new decision variables
  for (auto i = pu_i.cbegin(); i != pu_i.cend(); ++i)
    ptr->_ub.push_back(1.0);

  // add vtype for new decision variables
  for (auto i = pu_i.cbegin(); i != pu_i.cend(); ++i)
    ptr->_vtype.push_back(ptr->_vtype[0]);

  // add col ids for new decision variables
  for (auto i = pu_i.cbegin(); i != pu_i.cend(); ++i)
    ptr->_col_ids.push_back("c");

  // add new constraints to
  std::size_t A_row = (A_original_nrow - 1);
  for (std::size_t i = 0; i < (pu_i.size()); ++i) {
    // increment row
    ++A_row;
    // constraint to ensure that decision variable pu_i_zone_a_pu_j_zone_b
    // is less than or equal to pu_i_zone_a
    ptr->_A_i.push_back(A_row);
    ptr->_A_i.push_back(A_row);
    ptr->_A_j.push_back(A_original_ncol + i);
    ptr->_A_j.push_back(pu_i[i]);
    ptr->_A_x.push_back(1.0);
    ptr->_A_x.push_back(-1.0);
    ptr->_sense.push_back("<=");
    ptr->_rhs.push_back(0.0);
    ptr->_row_ids.push_back("c1");

    // constraint to ensure that decision variable pu_i_zone_a_pu_j_zone_b is
    // less than or equal to pu_j_zone_b
    ++A_row;
    ptr->_A_i.push_back(A_row);
    ptr->_A_i.push_back(A_row);
    ptr->_A_j.push_back(A_original_ncol + i);
    ptr->_A_j.push_back(pu_j[i]);
    ptr->_A_x.push_back(1.0);
    ptr->_A_x.push_back(-1.0);
    ptr->_sense.push_back("<=");
    ptr->_rhs.push_back(0.0);
    ptr->_row_ids.push_back("c2");

    // constraint to ensure that decision variable pu_i_zone_a_pu_j_zone_b
    // is 1 iff pu_i_zone_a is 1 and pu_j_zone_b is 1
    // this is only needed when the penalties are negative
    if (pu_b[i] < 0) {
      ++A_row;
      ptr->_A_i.push_back(A_row);
      ptr->_A_i.push_back(A_row);
      ptr->_A_i.push_back(A_row);
      ptr->_A_j.push_back(A_original_ncol + i);
      ptr->_A_j.push_back(pu_i[i]);
      ptr->_A_j.push_back(pu_j[i]);
      ptr->_A_x.push_back(1.0);
      ptr->_A_x.push_back(-1.0);
      ptr->_A_x.push_back(-1.0);
      ptr->_sense.push_back(">=");
      ptr->_rhs.push_back(-1.0);
      ptr->_row_ids.push_back("c3");
    }
  }

  // return result
  return true;
}
