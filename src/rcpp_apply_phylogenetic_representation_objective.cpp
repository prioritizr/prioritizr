#include "prioritizr.h"
#include "optimization_problem.h" 

// [[Rcpp::export]]
bool rcpp_apply_phylogenetic_representation_objective(SEXP x,
  Rcpp::NumericVector targets,
  Rcpp::NumericVector costs, 
  double budget,
  arma::sp_mat branch_matrix, 
  Rcpp::NumericVector branch_lengths) {
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  // model rhs
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_rhs.push_back(0.0);
  ptr->_rhs.push_back(budget);
  for (std::size_t i=0; i<branch_lengths.size(); ++i)
    ptr->_rhs.push_back(0.0);
  // model sense variables
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_sense.push_back(">=");
  ptr->_sense.push_back("<=");
  for (std::size_t i=0; i<branch_lengths.size(); ++i)
    ptr->_sense.push_back(">=");
  // add in small negative number to objective for planning unit variables to 
  // break ties in solution and select solution with cheapest cost
  double cost_scale = (1.0e-10/(*std::min_element(costs.begin(), costs.end())));
  for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
    ptr->_obj.push_back(costs[i] * cost_scale);
  // add in default feature weights (feature treated equally) to break 
  // ties in solution to maximize the number of features preserved
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_obj.push_back(1.0e-10);
  // add in branch lengths into objective function
  for (auto it = branch_lengths.begin(); it!=branch_lengths.end(); ++it)
    ptr->_obj.push_back(*it);  
  // add in upper and lower bounds for the decision variables representing if
  // each species is adequately conserved
  for (std::size_t i=ptr->_number_of_planning_units;
       i < (ptr->_number_of_planning_units+ptr->_number_of_features);
       ++i)
    ptr->_ub.push_back(1.0);  
  for (std::size_t i=ptr->_number_of_planning_units;
       i < (ptr->_number_of_planning_units+ptr->_number_of_features);
       ++i)
    ptr->_lb.push_back(0.0);
  // add in upper and lower bounds for the decision variables representing if
  // the branch is adequately conserved
  for (auto it = branch_lengths.begin(); it!=branch_lengths.end(); ++it)
    ptr->_ub.push_back(1.0);  
  for (auto it = branch_lengths.begin(); it!=branch_lengths.end(); ++it)
    ptr->_lb.push_back(0.0);
  // add in binary variable types for variables representing if each species is
  // adequately conserved
  for (std::size_t i=ptr->_number_of_planning_units;
       i < (ptr->_number_of_planning_units+ptr->_number_of_features);
       ++i)
    ptr->_vtype.push_back("B");
  // add in binary variable types for variables representing if branch is 
  // conserved
  for (auto it = branch_lengths.begin(); it!=branch_lengths.end(); ++it)
    ptr->_vtype.push_back("B");
  // add in model matrix values for species targets
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_A_i.push_back(i);
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_A_j.push_back(ptr->_number_of_planning_units+i);
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_A_x.push_back(-1.0 * targets[i]);
  // add in budget constraints
  for (std::size_t j=0; j<(ptr->_number_of_planning_units); ++j)
    ptr->_A_i.push_back(ptr->_number_of_features);
  for (std::size_t j=0; j<(ptr->_number_of_planning_units); ++j)
    ptr->_A_j.push_back(j);
  for (std::size_t j=0; j<(ptr->_number_of_planning_units); ++j)
    ptr->_A_x.push_back(costs[j]);
  // add in matrix values for phylogenetic representation
  for (std::size_t i=0; i<branch_lengths.size(); ++i) {
    ptr->_A_i.push_back(ptr->_number_of_features + 1 + i);
    ptr->_A_j.push_back(ptr->_number_of_planning_units + 
                        ptr->_number_of_features + i);  
    ptr->_A_x.push_back(-1.0);
  }
  for (arma::sp_mat::const_iterator it=branch_matrix.begin(); 
       it!=branch_matrix.end(); 
       ++it) {
    ptr->_A_i.push_back(ptr->_number_of_features + 1 + it.col());
    ptr->_A_j.push_back(ptr->_number_of_planning_units + it.row());  
    ptr->_A_x.push_back(1.0);
  }
  // add in col ids
  for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
    ptr->_col_ids.push_back("pu");
  for (std::size_t i=ptr->_number_of_planning_units;
       i<(ptr->_number_of_planning_units+ptr->_number_of_features);
       ++i)
    ptr->_col_ids.push_back("spp_met");
  for (auto it=branch_lengths.begin(); it!=branch_lengths.end(); ++it)
    ptr->_col_ids.push_back("branch_met");
  // add in row ids
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_row_ids.push_back("spp_target");
  ptr->_row_ids.push_back("budget");
  for (auto it=branch_lengths.begin(); it!=branch_lengths.end(); ++it)
    ptr->_row_ids.push_back("branch_target");
  // add model sense
  ptr->_modelsense="max";
  return true;
}
