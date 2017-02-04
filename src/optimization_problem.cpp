#include "optimization_problem.h"

// [[Rcpp::export]]
SEXP rcpp_new_optimization_problem(std::size_t nrow = 1000000, std::size_t ncol = 1000000, std::size_t ncell=100000) {
  OPTIMIZATIONPROBLEM* x = new OPTIMIZATIONPROBLEM(nrow, ncol, ncell);
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::XPtr<OPTIMIZATIONPROBLEM>(x, true);
  return(ptr);
}

// [[Rcpp::export]]
SEXP rcpp_predefined_optimization_problem(Rcpp::List l) {
  std::string modelsense = Rcpp::as<std::string>(l["modelsense"]);
  std::vector<std::size_t> A_i = Rcpp::as<std::vector<std::size_t>>(l["A_i"]);
  std::vector<std::size_t> A_j = Rcpp::as<std::vector<std::size_t>>(l["A_j"]);
  std::vector<double> A_x = Rcpp::as<std::vector<double>>(l["A_x"]);
  std::vector<double> obj = Rcpp::as<std::vector<double>>(l["obj"]);
  std::vector<double> lb = Rcpp::as<std::vector<double>>(l["lb"]);
  std::vector<double> ub = Rcpp::as<std::vector<double>>(l["ub"]);
  std::vector<double> rhs = Rcpp::as<std::vector<double>>(l["rhs"]);
  std::vector<std::string> sense = Rcpp::as<std::vector<std::string>>(l["sense"]);
  std::vector<std::string> vtype = Rcpp::as<std::vector<std::string>>(l["vtype"]);
  std::vector<std::size_t> pu_indices_in_obj = Rcpp::as<std::vector<std::size_t>>(l["pu_indices_in_obj"]);
  std::vector<std::string> row_ids = Rcpp::as<std::vector<std::string>>(l["row_ids"]);
  std::vector<std::string> col_ids = Rcpp::as<std::vector<std::string>>(l["col_ids"]);
  OPTIMIZATIONPROBLEM* x = new OPTIMIZATIONPROBLEM(modelsense,A_i,A_j,A_x,obj,lb,ub,rhs,sense,vtype,
                          pu_indices_in_obj,row_ids,col_ids);
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::XPtr<OPTIMIZATIONPROBLEM>(x, true);
  return(ptr);
}

// [[Rcpp::export]]
std::size_t rcpp_get_optimization_problem_ncol(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x)->ncol());
}

// [[Rcpp::export]]
std::size_t rcpp_get_optimization_problem_nrow(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x)->nrow());
}

// [[Rcpp::export]]
std::size_t rcpp_get_optimization_problem_ncell(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x)->ncell());
}

// [[Rcpp::export]]
Rcpp::List rcpp_get_optimization_problem_A(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x)->A());  
}

// [[Rcpp::export]]
std::string rcpp_get_optimization_problem_modelsense(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x)->_modelsense);
}

// [[Rcpp::export]]
std::vector<std::string> rcpp_get_optimization_problem_vtype(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x)->_vtype);
}

// [[Rcpp::export]]
std::vector<double> rcpp_get_optimization_problem_obj(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x)->_obj);  
}

// [[Rcpp::export]]
std::vector<double> rcpp_get_optimization_problem_rhs(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x)->_rhs);
}

// [[Rcpp::export]]
std::vector<std::string> rcpp_get_optimization_problem_sense(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x)->_sense);  
}

// [[Rcpp::export]]
std::vector<double> rcpp_get_optimization_problem_lb(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x)->_lb);  
}

// [[Rcpp::export]]
std::vector<double> rcpp_get_optimization_problem_ub(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x)->_ub);  
}
 
// [[Rcpp::export]]
std::vector<std::size_t> rcpp_get_optimization_problem_pu_indices_in_obj(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x)->_pu_indices_in_obj);
}

// [[Rcpp::export]]
std::vector<std::string> rcpp_get_optimization_problem_col_ids(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x)->_col_ids); 
}

// [[Rcpp::export]]
std::vector<std::string> rcpp_get_optimization_problem_row_ids(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x)->_row_ids); 
}

