#include "package.h"

// [[Rcpp::export]]
double rcpp_boundary(
  Rcpp::NumericVector edge_factor, Rcpp::NumericMatrix zones_matrix,
  arma::sp_mat boundary_matrix, Rcpp::NumericVector exposed_boundary,
  Rcpp::NumericVector total_boundary, Rcpp::NumericMatrix solution) {

  // initialization
  std::size_t n_z = edge_factor.size();
  std::size_t n_pu = total_boundary.size();
  double c1 = 0.0;
  double c2 = 0.0;
  double c3 = 0.0;

  // temporary variables
  std::size_t curr_i, curr_j;
  double curr_value;
  arma::sp_mat curr_matrix;

  // compute total boundary of the selected planning units
  for (std::size_t z = 0; z < n_z; ++z) {
    for (std::size_t i = 0; i < n_pu; ++i) {
      curr_value =
        solution(i, z) *
        zones_matrix(z, z) * (
          (total_boundary[i] - exposed_boundary[i]) +
          (exposed_boundary[i] * edge_factor[z])
        );
      c1 += curr_value;
    }
  }

  // substract shared boundary lengths
  for (std::size_t z1 = 0; z1 < n_z; ++z1) {
    for (std::size_t z2 = z1; z2 < n_z; ++z2) {
      // scale the boundary matrix using the zone's edge factor
      curr_matrix = boundary_matrix;
      curr_matrix *= zones_matrix(z1, z2);

      // extract elements
      for (arma::sp_mat::const_iterator it = curr_matrix.begin();
           it != curr_matrix.end(); ++it) {
        // get row and column indices for cell
        curr_i = it.row();
        curr_j = it.col();
        curr_value = *it;
        // process data based on cell
        if (std::abs(curr_value) > 1.0e-15) {
          if (z1 == z2) {
            // amount of shared boundary between two different planning units
            // in the same zone
            c2 -= (
              2.0 * curr_value * solution(curr_i, z1) * solution(curr_j, z2)
            );
          } else {
            // amount of shared boundary between two different planning units
            // in different zones
            c3 -= (
              2.0 * curr_value * solution(curr_i, z1) * solution(curr_j, z2)
            );
            c3 -= (
              2.0 * curr_value * solution(curr_i, z2) * solution(curr_j, z1)
            );
          }
        }
      }
    }
  }

  // return result
  return (c1 + c2 + c3);
}
