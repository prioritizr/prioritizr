#include "package.h"
#include "optimization_problem.h"
#include "functions.h"

// [[Rcpp::export]]
bool rcpp_apply_connectivity_penalties(SEXP x, double penalty,
                                       const Rcpp::List data) {

  /* The following code makes the following critical assumptions
   *
   * 1. data is a list of lists containing sparse matrices. The size of
   *    of each list in each element of data should be equal to the number
   *    of elements in data.
   *
   * 2. each sparse matrix in data a sparse matrix with only cells in either
   *    the upper or lower triangle and the diagonal filled in. If this
   *    condition is not met, then the matrix calculations will be incorrect.
   *
   * 3. The number of rows and columns in each sparse matrix is equal to
   *    ptr->_number_of_planning_units
   *
   * 4. The number of rows and columns in zones_matrix is equal to
   *    ptr->_number_of_zones
   *
   */

  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t A_original_ncol = ptr->_obj.size();
  std::size_t A_original_nrow = ptr->_rhs.size();

  // define variable to rescale penalty, thus
  // if the objective is to maximize benefit:
  //   the total amount of connectivity per planning unit/zone allocation is
  //   added to the benefits and the amount of connectivity between planning
  //   unit/zone allocations has postive contributions to the objective
  // otherwise, if the objective is to minimize costs:
  //   the total amount of boundary per planning unit/zone allocation is
  //   substracted from the costs and the amount of connectivity between
  //   planning unit/zone allocations has negative contributions to the
  //   objective
  //
  // note that mult is either 1 or -1 depending on whether primary objective
  // is to maximize or minimize
  double mult = 1.0;
  if (ptr->_modelsense == "min") {
    mult *= -1.0;
  }

  // convert the list of list of sparseMatrix objects to a Rcpp classes
  std::vector<std::vector<arma::sp_mat>> matrices;
  import_connectivity_matrix_list(data, matrices, true);

  // penalty values that are added to the planning unit/zone allocation costs
  std::vector<double> pu_zone_penalties(ptr->_number_of_planning_units *
                                        ptr->_number_of_zones, 0.0);

  // calculate total number of non-zero elements in matrices
  std::size_t n_non_zero = 0;
  for (std::size_t z1 = 0; z1 < ptr->_number_of_zones; ++z1)
    for (std::size_t z2 = z1; z2 < ptr->_number_of_zones; ++z2)
      n_non_zero += matrices[z1][z2].n_nonzero;

  // declare and initialize values for penalty data
  std::vector<std::size_t> pu_i;
  pu_i.reserve(n_non_zero);
  std::vector<std::size_t> pu_j;
  pu_j.reserve(n_non_zero);
  std::vector<double> pu_b;
  pu_b.reserve(n_non_zero);
  std::size_t curr_i, curr_j, curr_col1, curr_col2;
  double curr_value;
  arma::sp_mat curr_matrix;

  // extract penalty data from matrices
  for (std::size_t z1 = 0; z1 < ptr->_number_of_zones; ++z1) {
    for (std::size_t z2 = 0; z2 < ptr->_number_of_zones; ++z2) {
      // scale the boundary matrix using the zone's edge factor and weighting
      // in the zone matrix
      curr_matrix = matrices[z1][z2];
      curr_matrix *= penalty * mult;
      if (z1 != z2) {
        curr_matrix.diag().zeros();
      }
      // extract elements
      for (arma::sp_mat::const_iterator it = curr_matrix.begin();
           it != curr_matrix.end(); ++it) {
        // get row and column indices for cell
        curr_i = it.row();
        curr_j = it.col();
        curr_value = *it;
        if (std::abs(curr_value) > 1.0e-15) {
          if ((curr_i == curr_j) && (z1 == z2)) {
            // connectivity weight for a single planning unit allocation
            curr_col1 = (z1 * ptr->_number_of_planning_units) + curr_i;
            pu_zone_penalties[curr_col1] += curr_value;
          } else {
            // add data for a single variable in
            // representing this specific combination of allocations
            curr_col1 = (z1 * ptr->_number_of_planning_units) + curr_i;
            curr_col2 = (z2 * ptr->_number_of_planning_units) + curr_j;
            pu_i.push_back(curr_col1);
            pu_j.push_back(curr_col2);
            pu_b.push_back(curr_value);
          }
        }
      }
    }
  }

  // add (or substract depending on previous scaling) the penalties for each
  // planning unit/zone allocation to the costs
  for (std::size_t i = 0;
       i < (ptr->_number_of_zones * ptr->_number_of_planning_units); ++i)
    ptr->_obj[i] += pu_zone_penalties[i];

  // add connectivities between planning unit/zone allocations to the
  // objective function
  for (auto i = pu_b.cbegin(); i != pu_b.cend(); ++i)
    ptr->_obj.push_back(*i);

  // add lb for new decision variables
  for (auto i = pu_i.cbegin(); i != pu_i.cend(); ++i)
    ptr->_lb.push_back(0.0);

  // add ub for new decision variables
  for (auto i = pu_i.cbegin(); i != pu_i.cend(); ++i)
    ptr->_ub.push_back(1.0);

  // add vtype for new decision variables
  for (auto i = pu_i.cbegin(); i != pu_i.cend(); ++i)
    ptr->_vtype.push_back("C");

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

    // add extra constraint to ensure that the shared boundary is for
    // pu_i_zone_a_pu_j_zone_b is included in the objective if needed
    if (((ptr->_modelsense == "min") && (pu_b[i] > 0)) ||
        ((ptr->_modelsense == "max") && (pu_b[i] < 0))) {
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

  // return success
  return true;
}
