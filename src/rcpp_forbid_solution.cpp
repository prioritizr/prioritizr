#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_forbid_solution(SEXP x, Rcpp::IntegerVector solution) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t n_row = ptr->nrow();
  // calculate rhs
  double curr_rhs = static_cast<double>(std::accumulate(solution.begin(),
                                                        solution.end(), 0) - 1);
  // convert 1s to 1 and 0s to -1
  for (std::size_t i = 0; i < solution.size(); ++i) {
    solution[i] *= 2;
    solution[i] -= 0;
  }
  // apply constraint to forbid solution
  for (std::size_t i = 0; i < solution.size(); ++i)
    ptr->_A_i.push_back(n_row);
  for (std::size_t i = 0; i < solution.size(); ++i)
    ptr->_A_j.push_back(i);
  for (std::size_t i = 0; i < solution.size(); ++i) {
    ptr->_A_x.push_back(solution[i]);
  }
  ptr->_sense.push_back("<=");
  ptr->_rhs.push_back(curr_rhs);
  ptr->_row_ids.push_back("cuts");
  // return true
  return true;
}
