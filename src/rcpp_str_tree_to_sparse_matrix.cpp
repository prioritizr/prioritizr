#include "package.h"
#include "rcpp_boundary_data.h"

// [[Rcpp::export]]
Rcpp::List rcpp_str_tree_to_sparse_matrix(Rcpp::List data) {
  // initialize objects
  const std::size_t n_elements = data.size();
  std::size_t rsize = Pow<2>(n_elements) / 2.0;
  std::vector<std::size_t> rows;
  rows.reserve(rsize);
  std::vector<std::size_t> cols;
  cols.reserve(rsize);
  Rcpp::IntegerVector curr_ints;
  SEXP curr_element;
  // fill rows and vectors
  for (std::size_t i = 0; i < n_elements; ++i) {
    curr_element = data[i];
    if (!Rf_isNull(curr_element)) {
      curr_ints = Rcpp::as<Rcpp::IntegerVector>(data[i]);
      for (std::size_t j = 0; j < static_cast<std::size_t>(curr_ints.size());
           ++j)
        rows.push_back(i + 1);
      for (std::size_t j = 0; j < static_cast<std::size_t>(curr_ints.size());
           ++j)
        cols.push_back(curr_ints[j]);
    }
  }
  // create values for matrix
  std::vector<std::size_t> values(rows.size(), 1.0);
  // create dimensions
  Rcpp::IntegerVector d(2);
  d[0] = n_elements + 1;
  d[1] = n_elements + 1;
  // return list
  return Rcpp::List::create(Rcpp::Named("i") = rows,
                            Rcpp::Named("j") = cols,
                            Rcpp::Named("x") = values,
                            Rcpp::Named("dims") = d);
}
