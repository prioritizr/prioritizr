#include "package.h"
#include "optimization_problem.h"
#include "moo-function.h" 
#include <numeric>
#include <string>
#include <iostream>

// [[Rcpp::export]]
SEXP rcpp_apply_hierachical_approach(
    SEXP current_ptrSEXP,
    SEXP prev_ptrSEXP,
    const Rcpp::NumericVector& prev_solution,
    double degradation = 0.0
) {
  
  // prep stuff
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> current_ptr(current_ptrSEXP);
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> prev_ptr(prev_ptrSEXP);
  
  const std::size_t n_pu = current_ptr->_number_of_planning_units;
  const std::size_t n_zone = current_ptr->_number_of_zones;
  const std::size_t pu_len = n_pu * n_zone;
  
  // Hierarchical start: calc previous objective value
  double prev_val = 0.0;
  for (std::size_t i = 0; i < static_cast<std::size_t>(prev_solution.size()); ++i)
    prev_val += prev_solution[i] * prev_ptr->_obj[i];
  
  // Build degradation constraint 
  double threshold;
  std::string sense;
  if (prev_ptr->_modelsense == "min") {
    threshold = prev_val * (1.0 + degradation);
    sense = "<=";
  } else if (prev_ptr->_modelsense == "max") {
    threshold = prev_val * (1.0 - degradation);
    sense = ">=";
  }
  
  // Append degradation constraint to previous A
  std::size_t new_row = prev_ptr->_row_ids.size();
  for (std::size_t j = 0; j < prev_ptr->_obj.size(); ++j) {
    prev_ptr->_A_i.push_back(new_row);
    prev_ptr->_A_j.push_back(j);
    prev_ptr->_A_x.push_back(prev_ptr->_obj[j]);
  }
  prev_ptr->_rhs.push_back(threshold);
  prev_ptr->_sense.push_back(sense);
  prev_ptr->_row_ids.push_back("row_" + std::to_string(new_row + 1));
  
  Rcpp::Rcout << "Hierarchical step: degradation constraint added, threshold = " << threshold << std::endl;
  
  // Create new optimization problem (
  OPTIMIZATIONPROBLEM* new_ptr = new OPTIMIZATIONPROBLEM(
    current_ptr->_modelsense,            // modelsense
    0,                                   // number_of_features
    n_pu,                                // number_of_planning_units
    n_zone,                              // number_of_zones
    std::vector<std::size_t>(),          // A_i
    std::vector<std::size_t>(),          // A_j
    std::vector<double>(),               // A_x
    std::vector<double>(),               // obj
    std::vector<double>(),               // lb
    std::vector<double>(),               // ub
    std::vector<double>(),               // rhs
    std::vector<std::string>(),          // sense
    std::vector<std::string>(),          // vtype
    std::vector<std::string>(),          // row_ids
    std::vector<std::string>(),          // col_ids
    true                                 // compressed_formulation
  );

// now need to put current and prev together. Start by combining them to list, so we can use ws methods
  std::vector<Rcpp::XPtr<OPTIMIZATIONPROBLEM>> h_list = {current_ptr, prev_ptr};
  
  // start with A
  std::size_t row_offset = 0;
  std::size_t extra_col_offset = 0;
  
  for (std::size_t pidx = 0; pidx < h_list.size(); ++pidx) {
    Rcpp::XPtr<OPTIMIZATIONPROBLEM> p = h_list[pidx];
    int n_extra_cols = static_cast<int>(p->_col_ids.size()) - pu_len;
    
    for (std::size_t k = 0; k < p->_A_i.size(); ++k) {
      int gi = static_cast<int>(p->_A_i[k]) + row_offset;
      int gj = static_cast<int>(p->_A_j[k]);
      double val = p->_A_x[k];
      
      if (gj < pu_len) {
        new_ptr->_A_i.push_back(gi);
        new_ptr->_A_j.push_back(gj);
        new_ptr->_A_x.push_back(val);
      } else {
        new_ptr->_A_i.push_back(gi);
        new_ptr->_A_j.push_back(pu_len + extra_col_offset + (gj - pu_len));
        new_ptr->_A_x.push_back(val);
      }
    }
    
    row_offset += static_cast<int>(p->_row_ids.size());
    extra_col_offset += n_extra_cols;
  }
  
  // adapt row and col ids
  new_ptr->_row_ids.resize(row_offset);
  for (std::size_t r = 0; r < row_offset; ++r)
    new_ptr->_row_ids[r] = "row_" + std::to_string(r + 1);
  
  new_ptr->_col_ids.resize(pu_len + extra_col_offset);
  for (std::size_t c = 0; c < pu_len + extra_col_offset; ++c)
    new_ptr->_col_ids[c] = "col_" + std::to_string(c + 1);
  
  // Copy bounds, vtypes, obj from current prob
  for (std::size_t i = 0; i < current_ptr->_lb.size(); ++i)
    new_ptr->_lb.push_back(current_ptr->_lb[i]);
  for (std::size_t i = 0; i < current_ptr->_ub.size(); ++i)
    new_ptr->_ub.push_back(current_ptr->_ub[i]);
  for (std::size_t i = 0; i < current_ptr->_vtype.size(); ++i)
    new_ptr->_vtype.push_back(current_ptr->_vtype[i]);
  for (std::size_t i = 0; i < current_ptr->_obj.size(); ++i)
    new_ptr->_obj.push_back(current_ptr->_obj[i]);
  
  // pad zeros for prev_ptr extra cols
  std::size_t prev_extra_cols = prev_ptr->_col_ids.size() > pu_len ?
  prev_ptr->_col_ids.size() - pu_len : 0;
  for (std::size_t i = 0; i < prev_extra_cols; ++i) {
    new_ptr->_lb.push_back(0.0);
    new_ptr->_ub.push_back(0.0);
    new_ptr->_vtype.push_back("C");
    new_ptr->_obj.push_back(0.0);
  }
  
  // append rhs and sense
  for (std::size_t i = 0; i < current_ptr->_rhs.size(); ++i)
    new_ptr->_rhs.push_back(current_ptr->_rhs[i]);
  for (std::size_t i = 0; i < current_ptr->_sense.size(); ++i)
    new_ptr->_sense.push_back(current_ptr->_sense[i]);
  
  for (std::size_t i = 0; i < prev_ptr->_rhs.size(); ++i)
    new_ptr->_rhs.push_back(prev_ptr->_rhs[i]);
  for (std::size_t i = 0; i < prev_ptr->_sense.size(); ++i)
    new_ptr->_sense.push_back(prev_ptr->_sense[i]);
  
  // debug prints
  Rcpp::Rcout << "New hierarchical model successfully created." << std::endl;
  Rcpp::Rcout << "Final dimensions: " << new_ptr->_row_ids.size() 
              << " rows, " << new_ptr->_col_ids.size() 
              << " columns, nnz=" << new_ptr->_A_x.size() << std::endl;
  
  return Rcpp::XPtr<OPTIMIZATIONPROBLEM>(new_ptr, true);
}