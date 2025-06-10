#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_surrounded_penalties(SEXP x, double penalty,
                                     const Rcpp::NumericMatrix zones_matrix,
                                     const arma::sp_mat data) {

  /* The following code makes the following critical assumptions
   *
   * 1. data is a sparse matrix with only cells in either the upper
   *    or lower triangle and the diagonal filled in. If this condition is not
   *    met, then the calculations will be incorrect.
   *
   * 2. The number of rows and columns in data is equal to
   *    ptr->_number_of_planning_units
   *
   * 3. The number of rows and columns in zones_matrix is equal to
   *    ptr->_number_of_zones
   *
   */

  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t A_original_ncol = ptr->_obj.size();
  std::size_t A_original_nrow = ptr->_rhs.size();
  std::size_t n_dv = ptr->_number_of_planning_units * ptr->_number_of_zones;

  // penalty values that are added to the planning unit/zone allocation costs
  std::vector<double> pu_zone_penalties(n_dv, 0.0);
  // note we start at one here so that later we don't need to increment
  // by a value of one when using these values to specify constraints
  std::vector<double> pu_zone_n_neighbors(n_dv, 1.0);

  // calculate number for preallocating vectors
  const std::size_t n_non_zero = data.n_nonzero *
                                 ptr->_number_of_zones *
                                 ptr->_number_of_zones;

  // declare and initialize values for penalty data
  std::size_t curr_i, curr_j, curr_col1, curr_col2;
  double curr_value;
  arma::sp_mat curr_matrix;
  boost::unordered_multimap<std::size_t, std::size_t> pu_boundary;
  pu_boundary.reserve(n_non_zero * 2);

  // extract penalty data from matrices
  for (std::size_t z1 = 0; z1 < ptr->_number_of_zones; ++z1) {
    for (std::size_t z2 = z1; z2 < ptr->_number_of_zones; ++z2) {

      // add penalties for total length to the pu_zone_penalties
      if (z1 == z2) {
        for (std::size_t i = 0; i < ptr->_number_of_planning_units; ++i) {
          curr_col1 = (z1 * ptr->_number_of_planning_units) + i;
          pu_zone_penalties[curr_col1] += penalty * zones_matrix(z1, z2);
          ++pu_zone_n_neighbors[curr_col1];
        }
      }

      // scale the boundary matrix using the zone's edge factor and weighting
      // in the zone matrix
      curr_matrix = data;
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
            pu_boundary.insert({curr_col1, curr_col2});
            /// store variable representing pj_z1_pi_z1
            pu_boundary.insert({curr_col2, curr_col1});
          } else {
            // amount of shared boundary between two different planning units
            // in two different zones
            /// pi_z1 boundary
            curr_col1 = (z1 * ptr->_number_of_planning_units) + curr_i;
            /// pj_z2 boundary
            curr_col2 = (z2 * ptr->_number_of_planning_units) + curr_j;
            // store variable representing pi_z1_pj_z2
            pu_boundary.insert({curr_col1, curr_col2});;
            // pi_z2 boundary
            curr_col1 = (z2 * ptr->_number_of_planning_units) + curr_i;
            // pj_z1 boundary
            curr_col2 = (z1 * ptr->_number_of_planning_units) + curr_j;
            // store variable representing pi_z2_pj_z1
            pu_boundary.insert({curr_col1, curr_col2});
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
  for (std::size_t i = 0; i < n_dv; ++i)
    ptr->_obj.push_back(-pu_zone_penalties[i]);

  // add lb for new decision variables
  for (std::size_t i = 0; i < n_dv; ++i)
    ptr->_lb.push_back(0.0);

  // add ub for new decision variables
  for (std::size_t i = 0; i < n_dv; ++i)
    ptr->_ub.push_back(1.0);

  // add vtype for new decision variables
  for (std::size_t i = 0; i < n_dv; ++i)
    ptr->_vtype.push_back(ptr->_vtype[0]);

  // add col ids for new decision variables
  for (std::size_t i = 0; i < n_dv; ++i)
    ptr->_col_ids.push_back("i");

  // add constraints for interior penalty variables
  std::size_t A_row = (A_original_nrow - 1);
  auto it = pu_boundary.equal_range(0);

  // if there is only a single zone, then we can use a simpler problem
  // formulation that involves adding in 2 constraints per planning unit.
  if (ptr->_number_of_zones == 1) {
    for (std::size_t i = 0; i < n_dv; ++i) {
      // find all edges for pu_i_zone_a
      it = pu_boundary.equal_range(i);

      // if planning unit has neighbors then...
      if (it.first != it.second) {
        /// add constraint for pu_i_zone_a_surrounded <= pu_i_zone_a
        ++A_row;
        ptr->_A_i.push_back(A_row);
        ptr->_A_j.push_back(i);
        ptr->_A_x.push_back(-1.0);
        ptr->_A_i.push_back(A_row);
        ptr->_A_j.push_back(A_original_ncol + i);
        ptr->_A_x.push_back(1.0);
        ptr->_sense.push_back("<=");
        ptr->_rhs.push_back(0.0);
        ptr->_row_ids.push_back("i1");

        /// add constraint for pu_i_zone_a_surrounded <= pu_j_zone_b
        for (auto itr2 = it.first; itr2 != it.second; ++itr2) {
          ++A_row;
          ptr->_A_i.push_back(A_row);
          ptr->_A_j.push_back(itr2->second);
          ptr->_A_x.push_back(-1.0);
          ptr->_A_i.push_back(A_row);
          ptr->_A_j.push_back(A_original_ncol + i);
          ptr->_A_x.push_back(1.0);
          ptr->_sense.push_back("<=");
          ptr->_rhs.push_back(0.0);
          ptr->_row_ids.push_back("i2");
        }
      }
    }
  } else {
  // if there is are multiple zones, then we need to use a more compact
  // formulation that involves adding in 1 constraint per planning unit.
    for (std::size_t i = 0; i < n_dv; ++i) {
      // find all edges for pu_i_zone_a
      it = pu_boundary.equal_range(i);

      // if planning unit has neighbors then...
      if (it.first != it.second) {

        // update counters
        ++A_row;

        /// add constraint for:
        // ((sum_{j in neighbors(i)} 1) * pu_i_zone_a_surrounded)
        // -pu_i_zone_a -
        // -sum_{j in neighbors(i)} sum_{b in Z} zones_{ab} pu_j_zone_b <= 0

        /// add constraint coef for pu_i_zone_a
        ptr->_A_i.push_back(A_row);
        ptr->_A_j.push_back(i);
        ptr->_A_x.push_back(-1.0);

        /// add constraint coefs for pu_j_zone_b
        for (auto itr2 = it.first; itr2 != it.second; ++itr2) {
          ptr->_A_i.push_back(A_row);
          ptr->_A_j.push_back(itr2->second);
          ptr->_A_x.push_back(-1.0);
        }

        // add constraint coef for pu_i_zone_a_surrounded
        ptr->_A_i.push_back(A_row);
        ptr->_A_j.push_back(A_original_ncol + i);
        ptr->_A_x.push_back(pu_zone_n_neighbors[i]);

        // add remaining constraint information
        ptr->_sense.push_back("<=");
        ptr->_rhs.push_back(0.0);
        ptr->_row_ids.push_back("i3");

      }
    }
  }

  // return success
  return true;
}
