#include "functions.h"

// import rij matrix
void import_rij(const Rcpp::List& rij_list, std::vector<arma::sp_mat>& rij) {
  arma::sp_mat m;
  rij.resize(rij_list.size());
  for (std::size_t i = 0; i < static_cast<std::size_t>(rij_list.size()); ++i) {
    m = Rcpp::as<arma::sp_mat>(rij_list[i]);
    rij[i] = m.t();
  }
  return;
}

// import list of list of matrices
void import_connectivity_matrix_list(
  const Rcpp::List& in,
  std::vector<std::vector<arma::sp_mat>>& out,
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

bool approx_equal(double x, double y) {
  return (std::abs(x - y) < 1.0e-15);
}

// shuffle a std::vector using R's RNG so that set.seed() works
// based on: https://gallery.rcpp.org/articles/stl-random-shuffle/
void r_random_shuffle(std::vector<std::size_t> &a) {
  // inialize variables
  int n = a.size();
  int j;

  // Fisher-Yates Shuffle Algorithm
  for (int i = 0; i < n - 1; i++) {
    j = i + rand_wrapper(n - i);
    std::swap(a[i], a[j]);
  }
  return;
}
