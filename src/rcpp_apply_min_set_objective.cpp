#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_min_set_objective(SEXP x, Rcpp::List targets_list,
                                      Rcpp::NumericMatrix costs) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  Rcpp::NumericVector targets_value = targets_list["value"];
  Rcpp::CharacterVector targets_sense = targets_list["sense"];
  // add objective function
  for (std::size_t i = 0; i < (ptr->_number_of_planning_units *
                               ptr->_number_of_zones); ++i) {
    if (Rcpp::NumericVector::is_na(costs[i])) {
      // NA costs for planning units in zones
      ptr->_obj.push_back(0.0);
      ptr->_lb[i] = 0;
      ptr->_ub[i] = 0;
    } else {
      ptr->_obj.push_back(costs[i]);
    }
  }
  if (!ptr->_compressed_formulation) {
    for (std::size_t i = 0; i < (ptr->_number_of_zones *
                                 ptr->_number_of_planning_units *
                                 ptr->_number_of_features); ++i)
      ptr->_obj.push_back(0.0);
  }
  // add target senses
  for (std::size_t i = 0; i < targets_value.size(); ++i)
    ptr->_sense.push_back(Rcpp::as<std::string>(targets_sense[i]));
  for (std::size_t i = 0; i < targets_value.size(); ++i)
    ptr->_rhs.push_back(targets_value[i]);
  // add row ids
  for (std::size_t i = 0; i < targets_value.size(); ++i)
    ptr->_row_ids.push_back("spp_target");
  // assign model sense
  ptr->_modelsense="min";
  // return succes
  return true;
}
