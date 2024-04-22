#include "optimization_problem.h"

// [[Rcpp::export]]
SEXP rcpp_new_optimization_problem(std::size_t nrow =  1000000,
                                   std::size_t ncol =  1000000,
                                   std::size_t ncell = 1000000) {
  OPTIMIZATIONPROBLEM* x = new OPTIMIZATIONPROBLEM(nrow, ncol, ncell);
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr =
    Rcpp::XPtr<OPTIMIZATIONPROBLEM>(x, true);
  return(ptr);
}

// [[Rcpp::export]]
SEXP rcpp_copy_optimization_problem(SEXP x) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> x_ptr =
    Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  // create new problem
  OPTIMIZATIONPROBLEM* y = new OPTIMIZATIONPROBLEM(
    x_ptr->_modelsense,
    x_ptr->_number_of_features,
    x_ptr->_number_of_planning_units,
    x_ptr->_number_of_zones,
    x_ptr->_A_i,
    x_ptr->_A_j,
    x_ptr->_A_x,
    x_ptr->_obj,
    x_ptr->_lb,
    x_ptr->_ub,
    x_ptr->_rhs,
    x_ptr->_sense,
    x_ptr->_vtype,
    x_ptr->_row_ids,
    x_ptr->_col_ids,
    x_ptr->_compressed_formulation
  );
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> y_ptr =
    Rcpp::XPtr<OPTIMIZATIONPROBLEM>(y, true);
  return(y_ptr);
}

// [[Rcpp::export]]
SEXP rcpp_predefined_optimization_problem(Rcpp::List l) {
  std::string modelsense = Rcpp::as<std::string>(l["modelsense"]);
  std::size_t number_of_features =
    Rcpp::as<std::size_t>(l["number_of_features"]);
  std::size_t number_of_planning_units =
    Rcpp::as<std::size_t>(l["number_of_planning_units"]);
  std::size_t number_of_zones =
    Rcpp::as<std::size_t>(l["number_of_zones"]);
  std::vector<std::size_t> A_i = Rcpp::as<std::vector<std::size_t>>(l["A_i"]);
  std::vector<std::size_t> A_j = Rcpp::as<std::vector<std::size_t>>(l["A_j"]);
  std::vector<double> A_x = Rcpp::as<std::vector<double>>(l["A_x"]);
  std::vector<double> obj = Rcpp::as<std::vector<double>>(l["obj"]);
  std::vector<double> lb = Rcpp::as<std::vector<double>>(l["lb"]);
  std::vector<double> ub = Rcpp::as<std::vector<double>>(l["ub"]);
  std::vector<double> rhs = Rcpp::as<std::vector<double>>(l["rhs"]);
  bool compressed_formulation = Rcpp::as<bool>(l["compressed_formulation"]);
  std::vector<std::string> sense =
    Rcpp::as<std::vector<std::string>>(l["sense"]);
  std::vector<std::string> vtype =
    Rcpp::as<std::vector<std::string>>(l["vtype"]);
  std::vector<std::string> row_ids =
    Rcpp::as<std::vector<std::string>>(l["row_ids"]);
  std::vector<std::string> col_ids =
    Rcpp::as<std::vector<std::string>>(l["col_ids"]);
  OPTIMIZATIONPROBLEM* x = new OPTIMIZATIONPROBLEM(
    modelsense, number_of_features, number_of_planning_units, number_of_zones,
    A_i, A_j, A_x, obj, lb, ub, rhs, sense, vtype, row_ids, col_ids,
    compressed_formulation
  );
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr =
    Rcpp::XPtr<OPTIMIZATIONPROBLEM>(x, true);
  return(ptr);
}

// [[Rcpp::export]]
Rcpp::List rcpp_optimization_problem_as_list(SEXP x) {
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr =
    Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  // create list
  return Rcpp::List::create(
    Rcpp::Named("modelsense") = ptr->_modelsense,
    Rcpp::Named("number_of_features") = ptr->_number_of_features,
    Rcpp::Named("number_of_planning_units") = ptr->_number_of_planning_units,
    Rcpp::Named("number_of_zones") = ptr->_number_of_zones,
    Rcpp::Named("A_i") =
      Rcpp::IntegerVector(ptr->_A_i.begin(), ptr->_A_i.end()),
    Rcpp::Named("A_j") =
      Rcpp::IntegerVector(ptr->_A_j.begin(), ptr->_A_j.end()),
    Rcpp::Named("A_x") = ptr->_A_x,
    Rcpp::Named("obj") = ptr->_obj,
    Rcpp::Named("lb") = ptr->_lb,
    Rcpp::Named("ub") = ptr->_ub,
    Rcpp::Named("rhs") = ptr->_rhs,
    Rcpp::Named("compressed_formulation") = ptr->_compressed_formulation,
    Rcpp::Named("sense") = ptr->_sense,
    Rcpp::Named("vtype") = ptr->_vtype,
    Rcpp::Named("row_ids") = ptr->_row_ids,
    Rcpp::Named("col_ids") = ptr->_col_ids);
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
std::size_t rcpp_get_optimization_problem_number_of_planning_units(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x)->_number_of_planning_units);
}

// [[Rcpp::export]]
std::size_t rcpp_get_optimization_problem_number_of_features(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x)->_number_of_features);
}

// [[Rcpp::export]]
std::size_t rcpp_get_optimization_problem_number_of_zones(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x)->_number_of_zones);
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
std::vector<std::string> rcpp_get_optimization_problem_col_ids(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x)->_col_ids);
}

// [[Rcpp::export]]
std::vector<std::string> rcpp_get_optimization_problem_row_ids(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x)->_row_ids);
}

// [[Rcpp::export]]
bool rcpp_get_optimization_problem_compressed_formulation(SEXP x) {
  return(Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x)->_compressed_formulation);
}

// extract multiple elements from a vector
template <typename T1, typename T2>
T1 extract_elements(T1 x, T2 &indices) {
  // initialize new vector
  T1 r(indices.size());
  // fill new vector
  for (std::size_t i = 0; i < indices.size(); ++i)
    r[i] = x[indices[i]];
  // return new vector
  return r;
}

// [[Rcpp::export]]
Rcpp::IntegerVector rcpp_set_optimization_problem_shuffled(
  SEXP x,
  std::vector<std::size_t> shuffle_key
) {
  // NOTICE //
  // here we assume that shuffle_key contains values from 1 to the
  // columns (decision variables) in the problem. E.g., a valid shuffle_key
  // might be [2, 1, 4, 5, 3] for a problem with 5 columns.
  // If this assumption is not met, then the code will produce invalid
  // output.

  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr =
    Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  // convert key from base-1 indexing to base-0 indexing
  for (auto& i : shuffle_key) i -= 1;
  // re-order columns in optimization problem according to shuffle key
  ptr->_obj =
    extract_elements<std::vector<double>, std::vector<std::size_t>>(
      ptr->_obj, shuffle_key
    );
  ptr->_col_ids =
    extract_elements<std::vector<std::string>, std::vector<std::size_t>>(
      ptr->_col_ids, shuffle_key
    );
  ptr->_vtype =
    extract_elements<std::vector<std::string>, std::vector<std::size_t>>(
      ptr->_vtype, shuffle_key
    );
  ptr->_lb =
    extract_elements<std::vector<double>, std::vector<std::size_t>>(
      ptr->_lb, shuffle_key
    );
  ptr->_ub =
    extract_elements<std::vector<double>, std::vector<std::size_t>>(
      ptr->_ub, shuffle_key
    );
  /// note that we use -1 below, because Rcpp::match outputs values under
  /// base-1 indexing for compatibolity with R::match() and we use base-0
  /// indexing for column indices in the constraint matrix (i.e., ptr->_A_j)
  Rcpp::IntegerVector new_j = Rcpp::match(
    Rcpp::IntegerVector(ptr->_A_j.begin(), ptr->_A_j.end()),
    Rcpp::IntegerVector(shuffle_key.begin(), shuffle_key.end())
  ) - 1;
  ptr->_A_j = std::vector<std::size_t>(new_j.begin(), new_j.end());
  // generate key so that we can later reorder the variables to the original
  // order (i.e., unshuffle the problem)
  std::vector<std::size_t> old_order(ptr->ncol());
  std::iota(old_order.begin(), old_order.end(), 0);
  Rcpp::IntegerVector reorder_key = Rcpp::match(
    Rcpp::IntegerVector(old_order.begin(), old_order.end()),
    Rcpp::IntegerVector(shuffle_key.begin(), shuffle_key.end())
  );
  // return key
  return reorder_key;
}
