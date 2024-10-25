#include "package.h"
#include "functions.h"

// [[Rcpp::export]]
Rcpp::NumericVector rcpp_absolute_amount_held_by_solution(
  const Rcpp::List rij_list,
  const Rcpp::List targets_list,
  const Rcpp::NumericMatrix solution) {
  // declare variables
  Rcpp::IntegerVector targets_feature = targets_list["feature"];
  Rcpp::List targets_zone = targets_list["zone"];
  Rcpp::IntegerVector curr_z;
  // import list of sparse matrices as a vector of sparse matrices
  // note that the matrices are transposed since armadillo doesn't support
  // row-major sparse matrices to make populating the problem object
  // more efficient
  std::vector<arma::sp_mat> rij;
  import_rij(rij_list, rij);
  // prepare solution matrix format
  arma::mat sol = Rcpp::as<arma::mat>(solution);
  // intialize variables
  std::size_t n_t = targets_feature.size();
  Rcpp::NumericVector out(n_t, 0.0);
  // calculate absolute amount held by solution
  for (std::size_t i = 0; i < n_t; ++i) {
    curr_z = Rcpp::as<Rcpp::IntegerVector>(targets_zone[i]) - 1;
    for (std::size_t z = 0; z < static_cast<std::size_t>(curr_z.size());
         ++z) {
        out[i] += arma::accu(
          rij[curr_z[z]].col(targets_feature[i] - 1) % sol.col(curr_z[z])
        );
      }
  }
  // return result
  return(out);
}
