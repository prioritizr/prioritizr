#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_boundary_penalties(SEXP x, double penalty,
                                   const Rcpp::NumericVector edge_factor,
                                   const Rcpp::NumericMatrix zones_matrix,
                                   const arma::sp_mat boundary_matrix,
                                   const Rcpp::NumericVector exposed_boundary,
                                   const Rcpp::NumericVector total_boundary) {

  /* The following code makes the following critical assumptions
   *
   * 1. boundary_matrix is a sparse matrix with only cells in either the upper
   *    or lower triangle and the diagonal filled in. If this condition is not
   *    met, then the boundary calculations will be incorrect.
   *
   * 2. exposed_boundary contains the amount of exposed boundary length
   *    for each planning unit.
   *
   * 3. total_boundary contains the amount of exposed boundary length
   *    for each planning unit. Thus:
   *    all(total_boundary >= exposed_boundary)
   *    colSums(boundary_matrix) <= total_boundary
   *
   * 4. the number of elements in edge_factor is equal to ptr->_number_of_zones
   *
   * 5. The number of rows and columns in boundary_matrix is equal to
   *    ptr->_number_of_planning_units
   *
   * 6. The number of rows and columns in zones_matrix is equal to
   *    ptr->_number_of_zones
   *
   */

  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t A_original_ncol = ptr->_obj.size();
  std::size_t A_original_nrow = ptr->_rhs.size();

  // penalty values that are added to the planning unit/zone allocation costs
  std::vector<double> pu_zone_penalties(ptr->_number_of_planning_units *
                                        ptr->_number_of_zones, 0.0);

  // calculate number for preallocating vectors
  const std::size_t n_non_zero = boundary_matrix.n_nonzero *
                                 ptr->_number_of_zones *
                                 ptr->_number_of_zones;

  // rescale penalty, thus
  // if the objective is to maximize benefit:
  //   the total amount of boundary per planning unit/zone allocation is
  //   substracted from the benefits and the shared boundaries between planning
  //   unit/zone allocations have postive contributions to the objective
  // otherwise, if the objective is to minimize costs:
  //   the total amount of boundary per planning unit/zone allocation is
  //   added to the costs and the shared boundaries between planning
  //   unit/zone allocations have negative contributions to the objective
  if (ptr->_modelsense == "max") {
    penalty *= -1.0;
  }

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
    for (std::size_t z2 = z1; z2 < ptr->_number_of_zones; ++z2) {

      // add penalties for total length to the pu_zone_penalties
      if (z1 == z2) {
        for (std::size_t i = 0; i < ptr->_number_of_planning_units; ++i) {
          curr_col1 = (z1 * ptr->_number_of_planning_units) + i;
          pu_zone_penalties[curr_col1] += penalty * zones_matrix(z1, z2) * (
            (total_boundary[i] - exposed_boundary[i]) +
            (exposed_boundary[i] * edge_factor[z1])
          );
        }
      }

      // scale the boundary matrix using the zone's edge factor and weighting
      // in the zone matrix
      curr_matrix = boundary_matrix;
      curr_matrix *= zones_matrix(z1, z2);
      curr_matrix *= penalty;

      // extract elements
      for (arma::sp_mat::const_iterator it = curr_matrix.begin();
           it != curr_matrix.end(); ++it) {
        // get row and column indices for cell
        curr_i = it.row();
        curr_j = it.col();
        curr_value = *it;
        if (
          (std::abs(curr_value) > 1.0e-15) &&
          (curr_i != curr_j)
        ) {
          if (z1 == z2) {
            // amount of shared boundary between two different planning units
            // in the same zone
            /// add pi_z1 boundary
            curr_col1 = (z1 * ptr->_number_of_planning_units) + curr_i;
            /// add pj_z1 boundary
            curr_col2 = (z1 * ptr->_number_of_planning_units) + curr_j;
            /// store variable representing pi_z1_pj_z1
            pu_i.push_back(curr_col1);
            pu_j.push_back(curr_col2);
            pu_b.push_back(curr_value * -2.0);
          } else {
            // amount of shared boundary between two different planning units
            // in two different zones
            /// pi_z1 boundary
            curr_col1 = (z1 * ptr->_number_of_planning_units) + curr_i;
            /// pj_z2 boundary
            curr_col2 = (z2 * ptr->_number_of_planning_units) + curr_j;
            // store variable representing pi_z1_pj_z2
            pu_i.push_back(curr_col1);
            pu_j.push_back(curr_col2);
            pu_b.push_back(curr_value * -2.0);
            // pi_z2 boundary
            curr_col1 = (z2 * ptr->_number_of_planning_units) + curr_i;
            // pj_z1 boundary
            curr_col2 = (z1 * ptr->_number_of_planning_units) + curr_j;
            // store variable representing pi_z2_pj_z1
            pu_i.push_back(curr_col1);
            pu_j.push_back(curr_col2);
            pu_b.push_back(curr_value * -2.0);
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

  // add shared boundaries between planning unit/zone allocations to the
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
    ptr->_col_ids.push_back("b");

  // add AND constraints for boundary penalty variables
  std::size_t A_row = (A_original_nrow - 1);
  for (std::size_t i = 0; i < (pu_i.size()); ++i) {
    // increment row
    ++A_row;
    // pu_i_zone_a_pu_j_zone_b <= pu_i_zone_a
    ptr->_A_i.push_back(A_row);
    ptr->_A_i.push_back(A_row);
    ptr->_A_j.push_back(A_original_ncol + i);
    ptr->_A_j.push_back(pu_i[i]);
    ptr->_A_x.push_back(1.0);
    ptr->_A_x.push_back(-1.0);
    ptr->_sense.push_back("<=");
    ptr->_rhs.push_back(0.0);
    ptr->_row_ids.push_back("b1");

    // pu_i_zone_a_pu_j_zone_b <= pu_j_zone_b
    ++A_row;
    ptr->_A_i.push_back(A_row);
    ptr->_A_i.push_back(A_row);
    ptr->_A_j.push_back(A_original_ncol + i);
    ptr->_A_j.push_back(pu_j[i]);
    ptr->_A_x.push_back(1.0);
    ptr->_A_x.push_back(-1.0);
    ptr->_sense.push_back("<=");
    ptr->_rhs.push_back(0.0);
    ptr->_row_ids.push_back("b2");

    // pu_i_zone_a_pu_j_zone_b - pu_i_zone_a - pu_j_zone_b >= -1
    if ((pu_b[i] > 0 && (ptr->_modelsense == "min")) ||
        (pu_b[i] < 0 && (ptr->_modelsense == "max"))) {
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
      ptr->_row_ids.push_back("b3");
    }
  }

  // return success
  return true;
}
