#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_min_shortfall_objective(SEXP x, Rcpp::List targets_list,
                                        Rcpp::NumericMatrix costs,
                                        Rcpp::NumericVector budget) {
  // initialize
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  Rcpp::NumericVector targets_value = targets_list["value"];
  Rcpp::CharacterVector targets_sense = targets_list["sense"];
  std::size_t A_extra_ncol;
  std::size_t A_extra_nrow;
  const std::size_t n_targets = targets_value.size();
  if (ptr->_compressed_formulation) {
    A_extra_ncol = 0;
    A_extra_nrow = 0;
  } else {
    A_extra_ncol = ptr->_number_of_zones * ptr->_number_of_planning_units *
                   ptr->_number_of_features;
    A_extra_nrow = *(ptr->_A_i.rbegin()) - n_targets + 1;
  }
  // model rhs
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_rhs.push_back(targets_value[i]);
  for (std::size_t z = 0; z < static_cast<std::size_t>(budget.size()); ++z)
    ptr->_rhs.push_back(budget[z]);
  // model sense variables
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_sense.push_back(Rcpp::as<std::string>(targets_sense[i]));
  for (std::size_t z = 0; z < static_cast<std::size_t>(budget.size()); ++z)
    ptr->_sense.push_back("<=");
  // add in small negative number to objective for planning unit variables to
  // break ties in solution and select solution with cheapest cost
  for (std::size_t z = 0; z < (ptr->_number_of_zones); ++z) {
    for (std::size_t j = 0; j < (ptr->_number_of_planning_units); ++j) {
      if (Rcpp::NumericMatrix::is_na(costs(j, z))) {
        ptr->_obj.push_back(0.0);
        ptr->_lb[(z * ptr->_number_of_planning_units) + j] = 0.0;
        ptr->_ub[(z * ptr->_number_of_planning_units) + j] = 0.0;
        costs(j, z) = 0.0;
      } else {
        ptr->_obj.push_back(0);
      }
    }
  }
  if (!ptr->_compressed_formulation)
    for (std::size_t i = 0; i < A_extra_ncol; ++i)
       ptr->_obj.push_back(0.0);
  // add target totals to convert total amounts to proportions
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_obj.push_back(targets_value[i] > 1.0e-5 ? 1.0 / targets_value[i] : 0);
  // add in upper and lower bounds for the decision variables representing if
  // each species is adequately conserved
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_ub.push_back(std::numeric_limits<double>::infinity());
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_lb.push_back(0.0);
  // add in binary variable types for variables representing if each species is
  // adequately conserved
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_vtype.push_back("C");
  // add in model matrix values for species targets
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_i.push_back(A_extra_nrow + i);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_j.push_back((ptr->_number_of_zones *
                        ptr->_number_of_planning_units) + A_extra_ncol + i);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_x.push_back(1.0);
  // add in budget constraints
  if (budget.size() == 1) {
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
      ptr->_A_x.push_back(costs(j, z));
    }
  }
  // add in row and col ids
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_col_ids.push_back("spp_met");
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_row_ids.push_back("spp_target");
  for (std::size_t i = 0; i < static_cast<std::size_t>(budget.size()); ++i)
    ptr->_row_ids.push_back("budget");
  // set model sense
  ptr->_modelsense = "min";
  // return success
  return true;
}
