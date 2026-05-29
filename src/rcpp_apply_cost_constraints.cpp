#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_cost_constraints(
  SEXP x,
  const Rcpp::NumericVector budget,
  const Rcpp::CharacterVector sense,
  const arma::sp_mat data
) {
  // initialize
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  const std::size_t n = static_cast<std::size_t>(budget.size());
  const std::size_t nz = ptr->_number_of_zones;
  const std::size_t i = ptr->_rhs.size();
  std::size_t j = 0;

  // determine if each constraints actually need to be applied
  double total_cost = 0.0;
  std::vector<double> total_zone_cost(nz, 0.0);
  for (std::size_t z = 0; z < nz; ++z) {
    total_zone_cost[z] = arma::accu(data.col(z));
    total_cost += total_zone_cost[z];
  }

  // apply constraints
  if (n == 1) {
    /// single cost constraint across all zones
    if (total_cost > 1.0e-15) {
      /// rhs
      ptr->_rhs.push_back(budget[0]);
      /// sense
      ptr->_sense.push_back(Rcpp::as<std::string>(sense[0]));
      /// row ids
      ptr->_row_ids.push_back("budget");
      /// model coefficients
      for (auto itr = data.begin(); itr != data.end(); ++itr) {
        if (std::abs(*itr) > 1.0e-15) {
          ptr->_A_i.push_back(i);
          ptr->_A_j.push_back(
            (itr.col() * ptr->_number_of_planning_units) +
            itr.row()
          );
          ptr->_A_x.push_back(*itr);
        }
      }
    }
  } else {
    /// cost constraint for each zone
    for (std::size_t z = 0; z < n; ++z) {
      if (total_zone_cost[z] > 1.0e-15) {
        /// rhs
        ptr->_rhs.push_back(budget[z]);
        /// sense
        ptr->_sense.push_back(Rcpp::as<std::string>(sense[z]));
        /// row ids
        ptr->_row_ids.push_back("budget");
        /// model coefficients
        for (auto itr = data.begin_col(z); itr != data.end_col(z); ++itr) {
          if (std::abs(*itr) > 1.0e-15) {
            ptr->_A_i.push_back(i + j);
            ptr->_A_j.push_back(
              (itr.col() * ptr->_number_of_planning_units) +
              itr.row()
            );
            ptr->_A_x.push_back(*itr);
          }
        }
        /// increment counter
        ++j;
      }
    }
  }

  // return success
  return true;
}
