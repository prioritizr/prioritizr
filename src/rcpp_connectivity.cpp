#include "package.h"
#include "functions.h"

// [[Rcpp::export]]
double rcpp_connectivity(
  Rcpp::List data, Rcpp::NumericMatrix solution) {

  // initialization
  std::size_t n_z = solution.ncol();
  double out = 0.0;
  arma::sp_mat curr_matrix;

  // convert the list of list of sparseMatrix objects to a Rcpp classes
  std::vector<std::vector<arma::sp_mat>> matrices;
  import_connectivity_matrix_list(data, matrices, true);

  // extract penalty data from matrices
  for (std::size_t z1 = 0; z1 < solution.ncol(); ++z1) {
    for (std::size_t z2 = 0; z2 < solution.ncol(); ++z2) {
      // extract connectivity matrix
      curr_matrix = matrices[z1][z2];

      // force diagonal to zero where zone1 != zone2
      // in other words, we assume that planning units should not be
      // allocated to multiple zones, and so the connectivity between
      // a single planning unit allocated to two zones should be zero
      if (z1 != z2) {
        curr_matrix.diag().zeros();
      }

      // set matrix cells to zero for planning units not selected in solution
      for (std::size_t i = 0; i < solution.nrow(); ++i)  {
        curr_matrix.row(i) *= solution(i, z1);
        curr_matrix.col(i) *= solution(i, z2);
      }

      // add connecitity values to running total
      out += arma::accu(curr_matrix);
    }
  }

  // return result
  return out;
}
