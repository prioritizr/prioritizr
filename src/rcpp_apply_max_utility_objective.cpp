#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_max_utility_objective(SEXP x,
                                      Rcpp::NumericVector abundances,
                                      Rcpp::NumericVector costs,
                                      double budget) {
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);

  // initialize
  std::size_t A_extra_ncol;
  std::size_t A_extra_nrow;
  if (ptr->_compressed_formulation) {
    A_extra_ncol = 0;
    A_extra_nrow = 0;
  } else {
    A_extra_ncol = ptr->_number_of_planning_units * ptr->_number_of_features;
    A_extra_nrow = *(ptr->_A_i.rbegin()) - ptr->_number_of_features + 1;
  }

  // model rhs
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_rhs.push_back(0.0);
  ptr->_rhs.push_back(budget);
  // model sense variables
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_sense.push_back("=");
  ptr->_sense.push_back("<=");
  // add in small negative number to objective for planning unit variables to
  // break ties in solution and select solution with cheapest cost
  double cost_scale = (1.0e-10/(*std::min_element(costs.begin(), costs.end())));
  for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
    ptr->_obj.push_back(costs[i] * cost_scale);
  if (!ptr->_compressed_formulation)
    for (std::size_t i=0; i<A_extra_ncol; ++i)
       ptr->_obj.push_back(0.0);
  // add in default feature weights (features weighted equally by default)
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_obj.push_back(1.0);
  // add in upper bounds representing the maximum abundances for each feature
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_ub.push_back(abundances[i]);
  // add in lower bounds as zero
for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_lb.push_back(0.0);
  // add continuous variables representing how much of each species is
  // conserved
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_vtype.push_back("C");
  // add in model matrix values for feature abundances
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_A_i.push_back(A_extra_nrow + i);
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_A_j.push_back(ptr->_number_of_planning_units + A_extra_ncol + i);
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_A_x.push_back(-1.0);
  // add in budget constraints
    for (std::size_t j=0; j<(ptr->_number_of_planning_units); ++j)
      ptr->_A_i.push_back(A_extra_nrow + ptr->_number_of_features);
  for (std::size_t j=0; j<(ptr->_number_of_planning_units); ++j)
    ptr->_A_j.push_back(j);
  for (std::size_t j=0; j<(ptr->_number_of_planning_units); ++j)
    ptr->_A_x.push_back(costs[j]);
  // add in row and col ids
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_col_ids.push_back("amount");
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_row_ids.push_back("spp_amount");
  ptr->_row_ids.push_back("budget");
  ptr->_modelsense="max";
  return true;
}
