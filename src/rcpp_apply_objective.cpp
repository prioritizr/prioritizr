#include "optimization_problem.h"
 
// [[Rcpp::export]]
bool rcpp_apply_minimum_set_objective(SEXP x, Rcpp::NumericVector targets,
                                      Rcpp::NumericVector costs) {
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
    ptr->_obj.push_back(costs[i]);
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_sense.push_back(">=");
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_rhs.push_back(targets[i]);
  for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
    ptr->_col_ids.push_back("pu");
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_row_ids.push_back("spp_target");
  ptr->_modelsense="min";
  return true;
}

// [[Rcpp::export]]
bool rcpp_apply_maximum_coverage_objective(SEXP x, Rcpp::NumericVector targets,
                                           Rcpp::NumericVector costs, 
                                           double budget) {
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  // model rhs
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_rhs.push_back(0.0);
  ptr->_rhs.push_back(budget);
  // model sense variables
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_sense.push_back(">=");
  ptr->_sense.push_back("<=");
  // add in small number to objective for planning unit variables to break ties
  // in solution
  for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
    ptr->_obj.push_back(1.0e-10);
  // add in default species weights (species treated equally)
  for (std::size_t i=ptr->_number_of_planning_units;
       i < (ptr->_number_of_planning_units+ptr->_number_of_features); 
       ++i)
    ptr->_obj.push_back(1.0);
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
  // add in binary variable types for variables representing if each species is
  // adequately conserved
  for (std::size_t i=ptr->_number_of_planning_units;
       i < (ptr->_number_of_planning_units+ptr->_number_of_features);
       ++i)
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
  // add in row and col ids
  for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
    ptr->_col_ids.push_back("pu");
  for (std::size_t i=ptr->_number_of_planning_units;
       i<(ptr->_number_of_planning_units+ptr->_number_of_features);
       ++i)
    ptr->_col_ids.push_back("spp_met");
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_row_ids.push_back("spp_target");
  ptr->_row_ids.push_back("budget");
  ptr->_modelsense="max";
  return true;
}

// [[Rcpp::export]]
bool rcpp_apply_target_weights(SEXP x, Rcpp::NumericVector weights) {
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_obj[i+ptr->_number_of_planning_units] = weights[i];
  return true;
}
