#include "package.h"
#include "optimization_problem.h"
#include "moo-function.h"
#include <numeric>
#include <string>
#include <iostream>

// [[Rcpp::export]]
SEXP rcpp_apply_hierachical_approach2(
    SEXP current_ptrSEXP,
    SEXP prev_ptrSEXP,
    const Rcpp::NumericVector& prev_solution,
    double degradation = 0.0
) {

  // prep stuff
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> opt_current(current_ptrSEXP);
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> opt_prev(prev_ptrSEXP);

  std::vector<Rcpp::XPtr<OPTIMIZATIONPROBLEM>> opt = {opt_current, opt_prev};
  const std::size_t n = opt.size();

  const std::size_t n_pu = opt_current->_number_of_planning_units;
  const std::size_t n_zone = opt_current->_number_of_zones;
  const std::size_t n_status = n_pu * n_zone;

  // calculate previous objective value (do this first, so we calculate offsets correctly after)
  double prev_val = 0.0;
  for (std::size_t i = 0; i < static_cast<std::size_t>(prev_solution.size()); ++i)
    prev_val += prev_solution[i] * opt_prev->_obj[i];

  // Build degradation constraint
  double threshold;
  std::string sense;
  if (opt_prev->_modelsense == "min") {
    threshold = prev_val * (1.0 + degradation);
    sense = "<=";
  } else {
    threshold = prev_val * (1.0 - degradation);
    sense = ">=";
  }

  // Append degradation constraint to previous A
  std::size_t new_row = opt_prev->_row_ids.size();
  for (std::size_t j = 0; j < opt_prev->_obj.size(); ++j) {
    opt_prev->_A_i.push_back(new_row);
    opt_prev->_A_j.push_back(j);
    opt_prev->_A_x.push_back(opt_prev->_obj[j]);
  }

  // adapt rhs, sense and row_ids (rest stays the same)
  opt_prev->_rhs.push_back(threshold);
  opt_prev->_sense.push_back(sense);
  opt_prev->_row_ids.push_back("row_" + std::to_string(new_row + 1));

  // define counters to store object sizes
  std::vector<std::size_t> opt_n_ncol(n);
  std::vector<std::size_t> opt_n_nrow(n);
  std::vector<std::size_t> opt_n_A(n);

  for (std::size_t i = 0; i < n; ++i) {
    opt_n_ncol[i] = opt[i]->_col_ids.size();
    opt_n_nrow[i] = opt[i]->_row_ids.size();
    opt_n_A[i] = opt[i]->_A_i.size();
  }

  // define offset variables for rows, columns, and A
  std::vector<std::size_t> opt_row_offset(n,0);
  std::vector<std::size_t> opt_col_offset(n,0);
  std::vector<std::size_t> opt_A_offset(n,0);
  for (std::size_t i = 1; i < n; ++i) {
    opt_row_offset[i] = opt_row_offset[i-1] + opt_n_nrow[i-1];
    opt_col_offset[i] = opt_col_offset[i-1] + opt_n_ncol[i-1] - n_status;
    opt_A_offset[i]   = opt_A_offset[i-1] + opt_n_A[i-1];
  }

  // compute dimensions for combined problem
  const std::size_t mopt_ncol = std::accumulate(opt_n_ncol.begin(), opt_n_ncol.end(), 0) - ((n-1)*n_status);
  const std::size_t mopt_nrow = std::accumulate(opt_n_nrow.begin(), opt_n_nrow.end(), 0);
  const std::size_t mopt_n_A   = std::accumulate(opt_n_A.begin(), opt_n_A.end(), 0);

  // initialize optimization problem that has the current model sense and placeholders for the rest
  OPTIMIZATIONPROBLEM* mopt = new OPTIMIZATIONPROBLEM(
    opt_current->_modelsense,
    opt_current->_number_of_features + opt_prev->_number_of_features, // sum features
    n_pu,
    n_zone,
    std::vector<std::size_t>(mopt_n_A),   // A_i
    std::vector<std::size_t>(mopt_n_A),   // A_j
    std::vector<double>(mopt_n_A),        // A_x
    std::vector<double>(mopt_ncol,0.0),   // obj
    std::vector<double>(mopt_ncol),       // lb
    std::vector<double>(mopt_ncol),       // ub
    std::vector<double>(mopt_nrow),       // rhs
    std::vector<std::string>(mopt_nrow),  // sense
    std::vector<std::string>(mopt_ncol),  // vtype
    std::vector<std::string>(mopt_nrow),  // row_ids
    std::vector<std::string>(mopt_ncol),  // col_ids
    true                                  // compressed_formulation
  );

  // A (i,j,x)
  for (std::size_t i = 0; i < n; ++i) {
    for (std::size_t j = 0; j < opt[i]->_A_i.size(); ++j) {
      mopt->_A_i[j + opt_A_offset[i]] = opt[i]->_A_i[j] + opt_row_offset[i];
      mopt->_A_x[j + opt_A_offset[i]] = opt[i]->_A_x[j];
    }
    double curr_offset;
    for (std::size_t j = 0; j < opt[i]->_A_j.size(); ++j) {
      curr_offset = opt_col_offset[i] * static_cast<double>(opt[i]->_A_j[j] >= n_status);
      mopt->_A_j[j + opt_A_offset[i]] = opt[i]->_A_j[j] + curr_offset;
    }
  }

  // obj
  std::copy(opt_current->_obj.begin(), opt_current->_obj.end(), mopt->_obj.begin());
  // append zeros for any extra columns in previous problem
  for (std::size_t j = n_status; j < opt_prev->_col_ids.size(); ++j)
    mopt->_obj[j] = 0.0;

  // lb
  std::copy(opt[0]->_lb.begin(), opt[0]->_lb.end(), mopt->_lb.begin());
  for (std::size_t i = 1; i < n; ++i)
    std::copy(opt[i]->_lb.begin() + n_status, opt[i]->_lb.end(),
              mopt->_lb.begin() + n_status + opt_col_offset[i]);

  // ub
  std::copy(opt[0]->_ub.begin(), opt[0]->_ub.end(), mopt->_ub.begin());
  for (std::size_t i = 1; i < n; ++i)
    std::copy(opt[i]->_ub.begin() + n_status, opt[i]->_ub.end(),
              mopt->_ub.begin() + n_status + opt_col_offset[i]);

  // vtype
  std::copy(opt[0]->_vtype.begin(), opt[0]->_vtype.end(), mopt->_vtype.begin());
  for (std::size_t i = 1; i < n; ++i)
    if (opt_n_ncol[i] > n_status)
      std::copy(opt[i]->_vtype.begin() + n_status, opt[i]->_vtype.end(),
                mopt->_vtype.begin() + n_status + opt_col_offset[i]);

    // col_ids
    std::copy(opt[0]->_col_ids.begin(), opt[0]->_col_ids.end(), mopt->_col_ids.begin());
    for (std::size_t i = 1; i < n; ++i)
      if (opt_n_ncol[i] > n_status)
        std::copy(opt[i]->_col_ids.begin() + n_status, opt[i]->_col_ids.end(),
                  mopt->_col_ids.begin() + n_status + opt_col_offset[i]);

      // rhs and sense
      for (std::size_t i=0; i<n; ++i) {
        std::copy(opt[i]->_rhs.begin(), opt[i]->_rhs.end(), mopt->_rhs.begin() + opt_row_offset[i]);
        std::copy(opt[i]->_sense.begin(), opt[i]->_sense.end(), mopt->_sense.begin() + opt_row_offset[i]);
      }

      // row_ids
      for (std::size_t i=0; i<n; ++i)
        std::copy(opt[i]->_row_ids.begin(), opt[i]->_row_ids.end(), mopt->_row_ids.begin() + opt_row_offset[i]);

      return Rcpp::XPtr<OPTIMIZATIONPROBLEM>(mopt, true);
}
