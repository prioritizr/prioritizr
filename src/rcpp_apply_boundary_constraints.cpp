#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_boundary_constraints(SEXP x, double threshold,
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
  std::size_t n_dv = ptr->_number_of_planning_units * ptr->_number_of_zones;

  // threshold values that are added to the planning unit/zone allocation costs
  std::vector<double> pu_zone_penalties(ptr->_number_of_planning_units *
                                        ptr->_number_of_zones, 0.0);

  // calculate number for preallocating vectors
  const std::size_t n_non_zero = boundary_matrix.n_nonzero *
                                 ptr->_number_of_zones *
                                 ptr->_number_of_zones;

  // declare and initialize values for data
  std::size_t curr_i, curr_j, curr_col1, curr_col2;
  double curr_value;
  arma::sp_mat curr_matrix;
  boost::unordered_multimap<std::size_t, std::pair<std::size_t, double>> pu_boundary;
  pu_boundary.reserve(n_non_zero * 2);

  // extract data from matrices
  for (std::size_t z1 = 0; z1 < ptr->_number_of_zones; ++z1) {
    for (std::size_t z2 = z1; z2 < ptr->_number_of_zones; ++z2) {

      // add penalties for total length to the pu_zone_penalties
      if (z1 == z2) {
        for (std::size_t i = 0; i < ptr->_number_of_planning_units; ++i) {
          curr_col1 = (z1 * ptr->_number_of_planning_units) + i;
          pu_zone_penalties[curr_col1] += zones_matrix(z1, z2) * (
            (total_boundary[i] - exposed_boundary[i]) +
            (exposed_boundary[i] * edge_factor[z1])
          );
        }
      }

      // scale the boundary matrix using the zone's edge factor and weighting
      // in the zone matrix
      curr_matrix = boundary_matrix;
      curr_matrix *= zones_matrix(z1, z2);

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
            pu_boundary.insert({
              curr_col1,
              std::pair<std::size_t, double>(curr_col2, curr_value)
            });
            /// store variable representing pj_z1_pi_z1
            pu_boundary.insert({
              curr_col2,
              std::pair<std::size_t, double>(curr_col1, curr_value)
            });
          } else {
            // amount of shared boundary between two different planning units
            // in two different zones
            /// pi_z1 boundary
            curr_col1 = (z1 * ptr->_number_of_planning_units) + curr_i;
            /// pj_z2 boundary
            curr_col2 = (z2 * ptr->_number_of_planning_units) + curr_j;
            // store variable representing pi_z1_pj_z2
            pu_boundary.insert({
              curr_col1,
              std::pair<std::size_t, double>(curr_col2, curr_value)
            });
            // pi_z2 boundary
            curr_col1 = (z2 * ptr->_number_of_planning_units) + curr_i;
            // pj_z1 boundary
            curr_col2 = (z1 * ptr->_number_of_planning_units) + curr_j;
            // store variable representing pi_z2_pj_z1
            pu_boundary.insert({
              curr_col1,
              std::pair<std::size_t, double>(curr_col2, curr_value)
            });
          }
        }
      }
    }
  }

  // calculate rescaling factor
  double max_threshold_boundary = *std::max_element(
    pu_zone_penalties.begin(), pu_zone_penalties.end()
  );

  // rescale penalties
  if (max_threshold_boundary > 10000.0) {
    for (std::size_t i = 0; i < n_dv; ++i)
      pu_zone_penalties[i] =
        10000.0 * (pu_zone_penalties[i] / max_threshold_boundary);
    for (auto itr = pu_boundary.begin(); itr != pu_boundary.end(); ++itr) {
      itr->second.second =
        10000.0 * (itr->second.second / max_threshold_boundary);
    }
  }

  // add obj for new decision variables
  for (std::size_t i = 0; i < n_dv; ++i)
    ptr->_obj.push_back(0.0);

  // add lb for new decision variables
  for (std::size_t i = 0; i < n_dv; ++i)
    ptr->_lb.push_back(0.0);

  // add ub for new decision variables
  for (std::size_t i = 0; i < n_dv; ++i)
    ptr->_ub.push_back(1.0);

  // add vtype for new decision variables
  for (std::size_t i = 0; i < n_dv; ++i)
    ptr->_vtype.push_back("C");

  // add col ids for new decision variables
  for (std::size_t i = 0; i < n_dv; ++i)
    ptr->_col_ids.push_back("bc");

  // add constraints for alternative boundary threshold variables
  std::size_t A_row = (A_original_nrow - 1);
  auto it = pu_boundary.equal_range(0);
  for (std::size_t i = 0; i < n_dv; ++i) {

    // increment counter
    ++A_row;

    // add constraint to ensure that boundary variable for pu_i_zone_a
    // is <= pu_i_zone_a
    ptr->_A_i.push_back(A_row);
    ptr->_A_i.push_back(A_row);
    ptr->_A_j.push_back(i);
    ptr->_A_j.push_back(A_original_ncol + i);
    ptr->_A_x.push_back(-1.0);
    ptr->_A_x.push_back(1.0);
    ptr->_sense.push_back("<=");
    ptr->_rhs.push_back(0.0);
    ptr->_row_ids.push_back("bc1");

    // increment counter
    ++A_row;

    // add constraint to ensure that the boundary variable for
    // pu_i_zone_a is <= the proportion of the edges that have a neighbor
    // initalize constraint
    ptr->_A_i.push_back(A_row);
    ptr->_A_j.push_back(A_original_ncol + i);
    ptr->_A_x.push_back(pu_zone_penalties[i]);

    // find all edges for pu_i_zone_a
    it = pu_boundary.equal_range(i);
    for (auto itr2 = it.first; itr2 != it.second; ++itr2) {
      ptr->_A_i.push_back(A_row);
      ptr->_A_j.push_back(itr2->second.first);
      ptr->_A_x.push_back(-itr2->second.second);
    }

    // add remaining constraint information
    ptr->_sense.push_back("<=");
    ptr->_rhs.push_back(0.0);
    ptr->_row_ids.push_back("bc2");

  }

  // add constraint to ensure that the sum of the boundary variables
  // minus is less than the particular threshold

  // increment counter
  ++A_row;

  for (std::size_t i = 0; i < n_dv; ++i) {
    ptr->_A_i.push_back(A_row);
    ptr->_A_i.push_back(A_row);
    ptr->_A_j.push_back(i);
    ptr->_A_j.push_back(A_original_ncol + i);
    ptr->_A_x.push_back(-threshold * pu_zone_penalties[i]);
    ptr->_A_x.push_back(pu_zone_penalties[i]);
  }
  ptr->_sense.push_back(">=");
  ptr->_rhs.push_back(0.0);
  ptr->_row_ids.push_back("bc3");

  // return success
  return true;
}
