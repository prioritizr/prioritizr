#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_max_phylo_objective(SEXP x,
  Rcpp::List targets_list, Rcpp::NumericMatrix costs,
  Rcpp::NumericVector budget, arma::sp_mat branch_matrix,
  Rcpp::NumericVector branch_lengths) {
  // initialize
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t A_extra_ncol;
  std::size_t A_extra_nrow;
  Rcpp::IntegerVector targets_feature = targets_list["feature"];
  Rcpp::NumericVector targets_value = targets_list["value"];
  Rcpp::CharacterVector targets_sense = targets_list["sense"];
  const std::size_t n_targets = targets_value.size();
  const std::size_t n_branches = branch_lengths.size();
  const std::size_t n_budgets = budget.size();
  if (ptr->_compressed_formulation) {
    A_extra_ncol = 0;
    A_extra_nrow = 0;
  } else {
    A_extra_ncol = ptr->_number_of_zones * ptr->_number_of_planning_units *
                   ptr->_number_of_features;
    A_extra_nrow = *(ptr->_A_i.rbegin()) - n_targets + 1;
  }
  // convert branch matrix from [feature,branch] format to [target,branch]
  // format. This means that meeting multiple targets for the same feature in
  // will contribute no additional benefit
  arma::sp_mat branch_matrix2 = arma::sp_mat(n_targets, n_branches);
  for (std::size_t i = 0; i < n_targets; ++i)
    branch_matrix2.row(i) = branch_matrix.row(targets_feature[i] - 1);
  // model rhs
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_rhs.push_back(0.0);
  for (std::size_t z = 0; z < n_budgets; ++z)
    ptr->_rhs.push_back(budget[z]);
  for (std::size_t i = 0; i < n_branches; ++i)
    ptr->_rhs.push_back(0.0);
  // model sense variables
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_sense.push_back(Rcpp::as<std::string>(targets_sense[i]));
  for (std::size_t z = 0; z < n_budgets; ++z)
    ptr->_sense.push_back("<=");
  for (std::size_t i = 0; i < n_branches; ++i)
    ptr->_sense.push_back(">=");
  // add in small negative number to objective for planning unit variables to
  // break ties in solution and select solution with cheapest cost
  double cost_scale = (-0.1 * Rcpp::min(branch_lengths)) /
                      Rcpp::sum(na_omit(costs));
  for (std::size_t z = 0; z < (ptr->_number_of_zones); ++z) {
    for (std::size_t j = 0; j < (ptr->_number_of_planning_units); ++j) {
      if (Rcpp::NumericMatrix::is_na(costs(j, z))) {
        ptr->_obj.push_back(0.0);
        ptr->_lb[(z * ptr->_number_of_planning_units) + j] = 0.0;
        ptr->_ub[(z * ptr->_number_of_planning_units) + j] = 0.0;
        costs(j, z) = 0.0;
      } else {
        ptr->_obj.push_back(costs(j, z) * cost_scale);
      }
    }
  }
  if (!ptr->_compressed_formulation)
    for (std::size_t i = 0; i < A_extra_ncol; ++i)
       ptr->_obj.push_back(0.0);
  // add in default feature weights (i.e. zero)
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_obj.push_back(0.0);
  // add in branch lengths into objective function
  for (auto it = branch_lengths.begin(); it!=branch_lengths.end(); ++it)
    ptr->_obj.push_back(*it);
  // add in upper and lower bounds for the decision variables representing if
  // each species is adequately conserved
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_ub.push_back(1.0);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_lb.push_back(0.0);
  // add in upper and lower bounds for the decision variables representing if
  // the branch is adequately conserved
  for (std::size_t i = 0; i < n_branches; ++i)
      ptr->_ub.push_back(1.0);
  for (std::size_t i = 0; i < n_branches; ++i)
      ptr->_lb.push_back(0.0);
  // add in binary variable types for variables representing if each species is
  // adequately conserved
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_vtype.push_back("B");
  // add in binary variable types for variables representing if branch is
  // conserved
  for (std::size_t i = 0; i < n_branches; ++i)
    ptr->_vtype.push_back("B");
  // add in model matrix values for species targets
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_i.push_back(A_extra_nrow + i);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_j.push_back((ptr->_number_of_zones *
                        ptr->_number_of_planning_units) + A_extra_ncol +
                        i);
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_A_x.push_back(-1.0 * targets_value[i]);
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
      ptr->_A_x.push_back(costs(j, z));
    }
  }
  // add in matrix values for phylogenetic representation
  std::size_t counter = A_extra_nrow + n_targets + n_budgets - 1;
  for (std::size_t i = 0; i < n_branches; ++i) {
    // initialize
    ++counter;
    // add in branch data
    for (auto itr = branch_matrix2.begin_col(i);
         itr != branch_matrix2.end_col(i); ++itr) {
      ptr->_A_i.push_back(counter);
      ptr->_A_j.push_back((ptr->_number_of_planning_units *
                          ptr->_number_of_zones) + A_extra_ncol + itr.row());
       ptr->_A_x.push_back(1.0);
    }
    // add in branch representation constraint
    ptr->_A_i.push_back(counter);
    ptr->_A_j.push_back((ptr->_number_of_planning_units *
                        ptr->_number_of_zones) + A_extra_ncol + n_targets + i);
    ptr->_A_x.push_back(-1.0);
  }
  // add in col ids
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_col_ids.push_back("spp_met");
  for (std::size_t i = 0; i < n_branches; ++i)
    ptr->_col_ids.push_back("branch_met");
  // add in row ids
  for (std::size_t i = 0; i < n_targets; ++i)
    ptr->_row_ids.push_back("spp_target");
  for (std::size_t i = 0; i < n_budgets; ++i)
    ptr->_row_ids.push_back("budget");
  for (std::size_t i = 0; i < n_branches; ++i)
    ptr->_row_ids.push_back("branch_target");
  // add model sense
  ptr->_modelsense="max";
  // return success
  return true;
}
