#include "package.h"
#include "optimization_problem.h"
#include "functions.h"

// [[Rcpp::export]]
Rcpp::List rcpp_convert_ref_point_method(
  SEXP x,
  Rcpp::CharacterVector mopt_modelsense,
  Rcpp::NumericMatrix mopt_obj,
  Rcpp::NumericVector weights,
  Rcpp::NumericVector ref_points,
  Rcpp::NumericVector sh_ub
) {
  // Initialization
  /// define counters
  const std::size_t n = mopt_modelsense.size();
  /// import optimization problem
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  /// store original problem size
  std::size_t A_ncol = ptr->ncol();
  std::size_t A_nrow = ptr->nrow();

  // Set model sense
  ptr->_modelsense = "min";

  // Define additional decision variables for shortfall variables
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_lb.push_back(0.0);
    ptr->_ub.push_back(sh_ub[i]);
    ptr->_vtype.push_back("C");
    ptr->_col_ids.push_back("rsh");
  }

  // Define additional decision variables for maximum value
  // compute upper bound
  double ub = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    ub = std::max(ub, weights[i] * sh_ub[i]);
  }
  // compute apply constraint
  ptr->_ub.push_back(ub);
  ptr->_lb.push_back(0.0);
  ptr->_vtype.push_back("C");
  ptr->_col_ids.push_back("mobj");

  // Specify default objective coefficients
  for (std::size_t i = 0; i < A_ncol; ++i) {
    ptr->_obj[i] = 0.0;
  }
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_obj.push_back(0.0);
  }
  ptr->_obj.push_back(0.0);

  // Add linear constraints for calculating shortfall of reference points
  for (std::size_t j = 0; j < A_ncol; ++j) {
    for (std::size_t i = 0; i < n; ++i) {
      if (std::abs(mopt_obj(i, j)) >= 1.0e-15) {
        ptr->_A_i.push_back(A_nrow + i);
        ptr->_A_j.push_back(j);
        ptr->_A_x.push_back(mopt_obj(i, j));
      }
    }
  }
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_A_i.push_back(A_nrow + i);
    ptr->_A_j.push_back(A_ncol + i);
    ptr->_A_x.push_back(mopt_modelsense[i] == "max" ? 1.0 : -1.0);
  }
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_rhs.push_back(ref_points[i]);
  }
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_sense.push_back(mopt_modelsense[i] == "max" ? ">=" : "<=");
  }
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_row_ids.push_back("rsh");
  }

  // Add linear constraints for calculating the maximum of weighted shortfalls
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_A_i.push_back(A_nrow + n + i);
    ptr->_A_j.push_back(A_ncol + n);
    ptr->_A_x.push_back(1.0);
    ptr->_A_i.push_back(A_nrow + n + i);
    ptr->_A_j.push_back(A_ncol + i);
    ptr->_A_x.push_back(-1.0 * weights[i]);
  }
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_rhs.push_back(0.0);
  }
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_sense.push_back("<=");
  }
  for (std::size_t i = 0; i < n; ++i) {
    ptr->_row_ids.push_back("max");
  }

  // Create vector with modelsense for reference point method
  Rcpp::CharacterVector rp_modelsense = {"min", "min"};

  // Create matrix with objective coefficients for reference point method
  Rcpp::NumericMatrix rp_obj(2, ptr->ncol());
  /// initialize all values to zero
  for (std::size_t i = 0; i < (ptr->ncol() * 2); ++i) {
    rp_obj[i] = 0.0;
  }
  /// first objective is to minimize weighted max
  rp_obj(0, ptr->ncol() - 1) = 1.0;
  /// second objective is to minimize weighted sum
  for (std::size_t i = 0; i < n; ++i) {
    rp_obj(1, A_ncol + i) = weights[i];
  }

  // return success
  return Rcpp::List::create(
    Rcpp::Named("obj") = rp_obj,
    Rcpp::Named("modelsense") = rp_modelsense,
    Rcpp::Named("opt") = x
  );
}
