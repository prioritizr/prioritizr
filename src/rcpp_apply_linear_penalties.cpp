#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_linear_penalties(
  SEXP x, const Rcpp::NumericVector penalty, const arma::sp_mat data) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t curr_i;
  std::size_t curr_j;
  std::size_t curr_col;
  double curr_value;

  // create a copy of penalties if re-scaling needed
  std::vector<double> penalty_std;
  penalty_std.assign(penalty.cbegin(), penalty.cend());

  // rescale penalty, thus
  // if the objective is to maximize benefit:
  //   the total penalty is substracted from the benefits associated with
  //   the planning unit/zone allocations
  // otherwise, if the objective is to minimize costs:
  //   the total penalty is added to the costs associated with
  //   the planning unit/zone allocations
  if (ptr->_modelsense == "max") {
    for (auto itr = penalty_std.begin(); itr != penalty_std.end(); ++itr)
      (*itr) *= -1.0;
  }

  // penalty values that are added to the planning unit/zone allocation costs
  std::vector<double> pu_zone_penalties(ptr->_number_of_planning_units *
                                        ptr->_number_of_zones, 0.0);

  // calculate penalties for each planning unit/zone allocation
  for (arma::sp_mat::const_iterator it = data.begin();
       it != data.end(); ++it) {
    // get row and column indices for cell
    curr_i = it.row();
    curr_j = it.col();
    curr_value = (*it) * penalty_std[curr_j];
    if (std::abs(curr_value) > 1.0e-15) {
      curr_col = (curr_j * ptr->_number_of_planning_units) + curr_i;
      pu_zone_penalties[curr_col] += curr_value;
    }
  }

  // add the penalties to the objective function
  for (std::size_t i = 0;
       i < (ptr->_number_of_zones * ptr->_number_of_planning_units); ++i)
    ptr->_obj[i] += pu_zone_penalties[i];

  // return success
  return true;
}
