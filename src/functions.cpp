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
