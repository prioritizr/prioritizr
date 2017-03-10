#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_min_set_objective(SEXP x, Rcpp::NumericVector targets,
                                      Rcpp::NumericVector costs) {
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
    ptr->_obj.push_back(costs[i]);
  if (!ptr->_compressed_formulation) {
    for (std::size_t i = 0;
         i < (ptr->_number_of_planning_units * ptr->_number_of_features); ++i)
        ptr->_obj.push_back(0.0);
  }
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_sense.push_back(">=");
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_rhs.push_back(targets[i]);
  for (std::size_t i=0; i<(ptr->_number_of_features); ++i)
    ptr->_row_ids.push_back("spp_target");
  ptr->_modelsense="min";
  return true;
}
