#include "package.h"
#include "functions.h"

// [[Rcpp::export]]
double rcpp_asym_connectivity(
  Rcpp::List data, Rcpp::NumericMatrix solution) {

  // initialization
  std::size_t n_z = solution.ncol();
  double out = 0.0;
  arma::sp_mat curr_matrix;

  // convert the list of list of sparseMatrix objects to a Rcpp classes
  std::vector<std::vector<arma::sp_mat>> matrices;
  import_connectivity_matrix_list(data, matrices, true);

  // extract penalty data from matrices
  for (std::size_t z1 = 0; z1 < n_z; ++z1) {
    for (std::size_t z2 = 0; z2 < n_z; ++z2) {
      // extract connectivity matrix
      curr_matrix = matrices[z1][z2];
      // add sum of connectivity values between pairs of planning
      // units where one unit is selected and the other one isn't
      for (arma::sp_mat::const_iterator it = curr_matrix.begin();
           it != curr_matrix.end(); ++it) {
        if ((it.row() == it.col()) && (z1 == z2)) {
          out += ((*it) * (solution(it.row(), z1)));
        } else {
          out +=
            ((*it) * (solution(it.row(), z1)) * (solution(it.col(), z2)));
        }
      }
    }
  }

  // return result
  return out;
}
