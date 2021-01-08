#include "package.h"

// [[Rcpp::export]]
double rcpp_boundary(
  Rcpp::NumericVector edge_factor, Rcpp::NumericMatrix zones_matrix,
  arma::sp_mat boundary_matrix, Rcpp::NumericMatrix solution) {

  // initialization
  std::size_t n_z = edge_factor.size();
  double c1 = 0.0;
  double c2 = 0.0;
  double c3 = 0.0;

  // temporary variables
  std::size_t curr_i, curr_j;
  double curr_value;
  arma::sp_mat curr_matrix;

  // calculate boundary statistics data from matrices
  for (std::size_t z1 = 0; z1 < n_z; ++z1) {
    for (std::size_t z2 = z1; z2 < n_z; ++z2) {
      // scale the boundary matrix using the zone's edge factor and weighting
      // in the zone matrix
      curr_matrix = boundary_matrix;
      curr_matrix *= zones_matrix(z1, z2);
      if (z1 == z2) {
        curr_matrix.diag() *= edge_factor[z1];
      } else {
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
            // amount of exposed boundary in planning unit with no neighbours
            c1 += (curr_value * solution(curr_i, z1));
          } else if (z1 == z2) {
            // amount of shared boundary between two different planning units
            // in the same zone
            c2 += (curr_value * solution(curr_i, z1));
            c2 += (curr_value * solution(curr_j, z2));
            c2 -= (2.0 * curr_value *
                    solution(curr_i, z1) *
                    solution(curr_j, z2));
          } else {
            // amount of shared boundary between two different planning units
            // in different zones
            c3 -= (2.0 * curr_value *
                   solution(curr_i, z1) *
                   solution(curr_j, z2));
            c3 -= (2.0 * curr_value *
                   solution(curr_i, z2) *
                   solution(curr_j, z1));
          }
        }
      }
    }
  }

  // return result
  return (c1 + c2 + c3);
}
