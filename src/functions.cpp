#include "functions.h"

// calculate euclidean distance
double distance(double x0, double y0, double x1, double y1) {
  return(sqrt(std::abs(Pow<2>(x0-x1)) + std::abs(Pow<2>(y0-y1))));
}

// import rij matrix
void import_rij(Rcpp::List& rij_list, std::vector<arma::sp_mat>& rij) {
  arma::sp_mat m;
  rij.resize(rij_list.size());
  for (std::size_t i = 0; i < rij_list.size(); ++i) {
    m = Rcpp::as<arma::sp_mat>(rij_list[i]);
    rij[i] = m.t();
  }
  return;
}

// import list of list of matrices
void import_connectivity_matrix_list(
  Rcpp::List& in, std::vector<std::vector<arma::sp_mat>>& out,
  bool full_matrix = true) {
  // initialize
  std::size_t n = in.size();
  out.resize(n);
  // preallocate sizes
  for (std::size_t i = 0; i < n; ++i)
    out[i].resize(n);
  // populate out with sparse matrices
  Rcpp::List l;
  for (std::size_t i = 0; i < n; ++i) {
    l = Rcpp::as<Rcpp::List>(in[i]);
    for (std::size_t j = (full_matrix ? 0 : i); j < n; ++j) {
      out[i][j] = Rcpp::as<arma::sp_mat>(l[j]);
    }
  }
  return;
}
