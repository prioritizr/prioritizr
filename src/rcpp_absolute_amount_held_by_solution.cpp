#include "package.h"
#include "functions.h"

// [[Rcpp::export]]
Rcpp::NumericVector rcpp_absolute_amount_held_by_solution(
  Rcpp::List rij_list, Rcpp::List targets_list, Rcpp::NumericMatrix solution) {
  // declare variables
  Rcpp::IntegerVector targets_feature = targets_list["feature"];
  Rcpp::NumericVector targets_value = targets_list["value"];
  Rcpp::List targets_zone = targets_list["zone"];
  Rcpp::IntegerVector curr_z;
  // import list of sparse matrices as a vector of sparse matrices
  // note that the matrices are transposed since armadillo doesn't support
  // row-major sparse matrices to make populating the problem object
  // more efficient
  std::vector<arma::sp_mat> rij;
  import_rij(rij_list, rij);
  // intialize variables
  std::size_t n_f = static_cast<std::size_t>(rij[0].n_cols);
  std::size_t n_pu = static_cast<std::size_t>(rij[0].n_rows);
  std::size_t n_z = static_cast<std::size_t>(rij_list.size());
  std::size_t n_t = targets_feature.size();
  // calculate matrix with target rij data
  arma::sp_mat m(n_t, n_pu * n_z);
  m.reserve(n_t, n_pu * n_z, 10000);
  for (std::size_t i = 0; i < n_t; ++i) {
    curr_z = Rcpp::as<Rcpp::IntegerVector>(targets_zone[i]) - 1;
    for (std::size_t z = 0; z < static_cast<std::size_t>(curr_z.size());
         ++z) {
      for (auto it = rij[curr_z[z]].begin_col(targets_feature[i] - 1);
           it != rij[curr_z[z]].end_col(targets_feature[i] - 1); ++it) {
        m(i, (curr_z[z] * n_pu) + it.row()) = *it;
      }
    }
  }
  // create matrix with solution data
  arma::vec s(n_pu * n_z);
  for (std::size_t i = 0; i < n_z; ++i)
    for (std::size_t j = 0; j < n_pu; ++j)
      s[(i * n_pu) + j] = solution(j, i);
  // calculate absolute amount held by solution
  arma::mat amount_held = m * s;
  Rcpp::NumericVector out = Rcpp::wrap(amount_held.col(0));
  return(out);
}
