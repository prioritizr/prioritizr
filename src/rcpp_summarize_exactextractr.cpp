#include "package.h"
#include "functions.h"

// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_summarize_exactextractr(Rcpp::List x,
                                                 std::size_t nrow,
                                                 std::size_t ncol,
                                                 std::string fun) {
  // initialization
  Rcpp::NumericMatrix out(nrow, ncol);
  Rcpp::DataFrame curr_df;
  Rcpp::DataFrame curr_matrix;
  Rcpp::NumericVector curr_props;
  Rcpp::NumericVector curr_values;
  Rcpp::NumericVector curr_mult_values;
  Rcpp::LogicalVector curr_is_not_na;
  double curr_prop_sum;
  std::size_t curr_df_nrow;
  const bool is_sum = fun == "sum";

  // verify that fun is valid
  if ((fun != "sum") && (fun != "mean"))
    Rcpp::stop("argument to fun must be mean or sum");

  // main processing
  for (std::size_t i = 0; i < nrow; ++i) {
    /// extract data
    curr_df = Rcpp::as<Rcpp::DataFrame>(x[i]);
    curr_props = curr_df[ncol];
    curr_df_nrow = curr_props.size();
    /// iterate over each layer column
    for (std::size_t j = 0; j < ncol; ++j) {
      /// extract values from j'th layer
      curr_values = curr_df[j];
      /// find out which values are not NA
      curr_is_not_na = !Rcpp::is_na(curr_values);
      /// store output value
      if (is_sum) {
        //// calculate weighted sum
        for (std::size_t k = 0; k < curr_df_nrow; ++k) {
          if (curr_is_not_na[k]) {
            out(i, j) += curr_props[k] * curr_values[k];
          }
        }
      } else {
        //// calculate weighted mean
        curr_prop_sum = 0.0;
        for (std::size_t k = 0; k < curr_df_nrow; ++k) {
          if (curr_is_not_na[k]) {
            out(i, j) += curr_props[k] * curr_values[k];
            curr_prop_sum += curr_props[k];
          }
        }
        out(i, j) =  out(i, j) / curr_prop_sum;
      }
    }
  }

  // exports
  return out;
}
