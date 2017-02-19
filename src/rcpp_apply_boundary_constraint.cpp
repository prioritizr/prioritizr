#include <RcppArmadillo.h>
#include "optimization_problem.h"

// [[Rcpp::export]]
bool rcpp_apply_clumping_constraint(SEXP x, 
                                    arma::sp_mat boundary_matrix, 
                                    double penalty, 
                                    double edge_factor) {
  
  /* The following code makes the following critical assumption
   * 
   * boundary_matrix is a sparse matrix with only cells in the upper triangle 
   * and the diagonal filled in. If this condition is not met, then the 
   * boundary calculations will be incorrect.
   */
  
  // initialization
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr = Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x);
  std::size_t A_original_ncol = ptr->_obj.size();
  std::size_t A_original_nrow = ptr->_rhs.size();  
  // apply penalties
  boundary_matrix *= penalty;
  boundary_matrix.diag() *= edge_factor;
  // if the objective is to maximise values then make boundaries negative
  if (ptr->_modelsense == "max")
    boundary_matrix *= -1.0;
  // calculate total boundaries for each planning unit
  // since boundary_matrix is a sparse matrix with only the upper diagonal
  // filled in we need to calculate the row sums and col sums and take
  // the maximum value.
  // the row and col sums are implemented usng via matrix multiplcation since
  // armadillo does not appear to be implemented in a more efficient method
  std::vector<double> total_boundaries;
  total_boundaries.reserve(ptr->_number_of_planning_units);
  {
    arma::vec total_boundaries_row = boundary_matrix * 
      arma::ones(ptr->_number_of_planning_units, 1);
    arma::vec total_boundaries_col = boundary_matrix * 
      arma::ones(1, ptr->_number_of_planning_units);
    for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
      total_boundaries.push_back(std::max(total_boundaries_row[i], 
                                          total_boundaries_col[i]));
  }
  
  // extract planning unit indices and shared boundaries from the matrix
  // since the matrix is symmetric, we will only extract data from the 
  // the upper diagonal so that we generate variables for pu_i_j and not 
  // pu_j_i as well
  std::vector<std::size_t> pu_i;
  pu_i.reserve(boundary_matrix.n_nonzero);
  std::vector<std::size_t> pu_j;
  pu_j.reserve(boundary_matrix.n_nonzero);
  std::vector<std::size_t> pu_b;
  pu_b.reserve(boundary_matrix.n_nonzero);
  for (auto it=boundary_matrix.begin(); it!=boundary_matrix.end();  ++it) {
    pu_i.push_back(it.row());
    pu_j.push_back(it.col());
    pu_b.push_back(*it);
  }
  // add total boundaries to planning unit costs in obj
  for (std::size_t i=0; i<(ptr->_number_of_planning_units); ++i)
    ptr->_obj[i] += total_boundaries[i];
  // add exposed boundaries to obj
  for (auto i=pu_b.cbegin(); i!=pu_b.cend(); ++i)
    ptr->_obj.push_back((*i) * -1.0);
  // add lb for new decision variables
  for (auto i=pu_i.cbegin(); i!=pu_i.cend(); ++i)
    ptr->_lb.push_back(0.0);
  // add ub for new decision variables
  for (auto i=pu_i.cbegin(); i!=pu_i.cend(); ++i)
    ptr->_ub.push_back(1.0);
  // add new constraints to 
  std::size_t A_row;
  for (std::size_t i=0; i<(pu_i.size()); ++i) {
    // calculate current row in A
    A_row = A_original_nrow + i;
    // constraint to ensure that decision variable pu_i_j is less than or 
    // equal to pu_i    
    ptr->_A_i.push_back(A_row); 
    ptr->_A_i.push_back(A_row); 
    ptr->_A_j.push_back(A_original_ncol + i); 
    ptr->_A_j.push_back(pu_i[i]);
    ptr->_A_x.push_back(1.0);
    ptr->_A_x.push_back(-1.0);    
    ptr->_sense.push_back("<=");
    ptr->_rhs.push_back(0.0);
    
    // constraint to ensure that decision variable pu_i_j is less than or 
    // equal to pu_j
    ptr->_A_i.push_back(A_row); 
    ptr->_A_i.push_back(A_row); 
    ptr->_A_j.push_back(A_original_ncol + i); 
    ptr->_A_j.push_back(pu_j[i]);
    ptr->_A_x.push_back(1.0);
    ptr->_A_x.push_back(-1.0);
    ptr->_sense.push_back("<=");
    ptr->_rhs.push_back(0.0);

    // constraint to ensure that decision variable pu_i_j is calculated
    // correctly. This is not needed if the pu_i and pu_j decsion variables
    // are binary.
    if (ptr->_sense[0] != "B") {
      ptr->_A_i.push_back(A_row);
      ptr->_A_i.push_back(A_row);
      ptr->_A_i.push_back(A_row);
      ptr->_A_j.push_back(A_original_ncol + i);
      ptr->_A_j.push_back(pu_i[i]);
      ptr->_A_j.push_back(pu_j[i]);
      ptr->_A_x.push_back(1.0);
      ptr->_A_x.push_back(-1.0);
      ptr->_A_x.push_back(-1.0);
      ptr->_sense.push_back(">=");
      ptr->_rhs.push_back(-1.0);      
    }
  }
  // return result
  return true;
}
