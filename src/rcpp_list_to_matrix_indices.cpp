#include "package.h"
#include "functions.h"
#include <vector>

// [[Rcpp::export]]
Rcpp::List rcpp_list_to_matrix_indices(Rcpp::List x, 
                                       std::size_t n_preallocate=10000) {
  // initialization
  std::vector<std::size_t> I;
  I.reserve(n_preallocate);
  std::vector<std::size_t> J;
  J.reserve(n_preallocate);

  // extract names from list
  std::vector<std::string> x_names = Rcpp::as<std::vector<std::string>>(x.names());
  
  // main processing
  int n;
  SEXP tmp;
  Rcpp::NumericVector v;
  for (std::size_t i=0; i!=x.size(); ++i) {
    n = std::stoi(x_names[i]);
    tmp = x[i];
    if (!Rf_isNull(tmp)) {
      v = Rcpp::as<Rcpp::NumericVector>(tmp);
      for (std::size_t j=0; j!=v.size(); ++j) {
        if (n!=v[j]) {
          I.push_back(v[j]);
          J.push_back(n);
        }
      }
    }
  }
  
  // exports
  std::vector<std::size_t> X(I.size(), 1);
  Rcpp::List lst=Rcpp::List::create(Rcpp::Named("i")=I, Rcpp::Named("j")=J, 
                                    Rcpp::Named("x")=X);
  return(lst);
}
