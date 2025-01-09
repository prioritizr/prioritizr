#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_min_penalties_objective(
  SEXP x,
  const Rcpp::List targets_list,
  const Rcpp::NumericMatrix costs,
  const Rcpp::NumericVector budget) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  Rcpp::NumericVector targets_value = targets_list["value"];
  Rcpp::CharacterVector targets_sense = targets_list["sense"];
  std::size_t A_extra_nrow;
  const std::size_t n_targets = targets_value.size();
  const std::size_t n_budgets = budget.size();
  if (ptr->_compressed_formulation) {
    A_extra_nrow = 0;
  } else {
    A_extra_nrow = *(ptr->_A_i.rbegin()) - n_targets + 1;
  }
  // add objective function
  for (std::size_t i = 0; i < (ptr->_number_of_planning_units *
                               ptr->_number_of_zones); ++i) {
    if (Rcpp::NumericVector::is_na(costs[i])) {
      ptr->_lb[i] = 0.0;
      ptr->_ub[i] = 0.0;
    }
    ptr->_obj.push_back(0.0);
  }
  if (!ptr->_compressed_formulation) {
    for (std::size_t i = 0; i < (ptr->_number_of_zones *
                                 ptr->_number_of_planning_units *
                                 ptr->_number_of_features); ++i)
      ptr->_obj.push_back(0.0);
  }
  // add target senses
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_sense.push_back(Rcpp::as<std::string>(targets_sense[i]));
    // add target rhs
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_rhs.push_back(targets_value[i]);
  // add target row ids
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_row_ids.push_back("spp_target");
  // add in budget constraints
  if (n_budgets == 1) {
    for (std::size_t i = 0;
         i < (ptr->_number_of_zones) * (ptr->_number_of_planning_units); ++i)
        ptr->_A_i.push_back(A_extra_nrow + n_targets);
  } else {
    for (std::size_t z = 0; z < (ptr->_number_of_zones); ++z)
      for (std::size_t j = 0; j < (ptr->_number_of_planning_units); ++j)
        ptr->_A_i.push_back(A_extra_nrow + n_targets + z);
  }
  for (std::size_t i = 0;
       i < (ptr->_number_of_zones) * (ptr->_number_of_planning_units); ++i)
    ptr->_A_j.push_back(i);
  for (std::size_t z = 0; z < (ptr->_number_of_zones); ++z) {
    for (std::size_t j = 0; j < (ptr->_number_of_planning_units); ++j) {
      ptr->_A_x.push_back(
        Rcpp::NumericMatrix::is_na(costs(j, z)) ? 0 : costs(j, z)
      );
    }
  }
  // budget rhs
  for (std::size_t z = 0; z < n_budgets; ++z)
    ptr->_rhs.push_back(budget[z]);
  // budget senses
  for (std::size_t z = 0; z < n_budgets; ++z)
    ptr->_sense.push_back("<=");
  // add budget row ids
  for (std::size_t z = 0; z < n_budgets; ++z)
    ptr->_row_ids.push_back("budget");
  // assign model sense
  ptr->_modelsense="min";
  // return succes
  return true;
}
