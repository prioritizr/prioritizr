#ifndef OPTIMIZATION_PROBLEM_H
#define OPTIMIZATION_PROBLEM_H

#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

#include <vector>
#include <algorithm>
 
class OPTIMIZATIONPROBLEM
{
  public:
    // constructor
    OPTIMIZATIONPROBLEM(){};
    OPTIMIZATIONPROBLEM(std::size_t nrow, std::size_t ncol, std::size_t ncell) {
      _A_i.reserve(ncell);
      _A_j.reserve(ncell);
      _A_x.reserve(ncell);
      _obj.reserve(ncol);
      _rhs.reserve(ncol);
      _vtype.reserve(ncol);
      _pu_indices_in_obj.reserve(ncol);
      _col_ids.reserve(ncol);
      _lb.reserve(nrow);
      _ub.reserve(nrow);
      _sense.reserve(nrow);
      _row_ids.reserve(nrow);
    };
    OPTIMIZATIONPROBLEM(std::string modelsense, 
                        std::vector<std::size_t> A_i,
                        std::vector<std::size_t> A_j,
                        std::vector<double> A_x,
                        std::vector<double> obj,
                        std::vector<double> lb,
                        std::vector<double> ub,
                        std::vector<double> rhs,
                        std::vector<std::string> sense,
                        std::vector<std::string> vtype,
                        std::vector<std::size_t> pu_indices_in_obj, 
                        std::vector<std::string> row_ids,
                        std::vector<std::string> col_ids) :
                        _modelsense(modelsense),
                        _A_i(A_i),
                        _A_j(A_j),
                        _A_x(A_x),
                        _obj(obj),
                        _lb(lb),
                        _ub(ub),
                        _rhs(rhs),
                        _sense(sense),
                        _vtype(vtype),
                        _pu_indices_in_obj(pu_indices_in_obj),
                        _row_ids(row_ids),
                        _col_ids(col_ids)
    {};
    // deconstructor
    ~OPTIMIZATIONPROBLEM(){};
    // fields
    std::string _modelsense;
    
    std::vector<std::size_t> _pu_indices_in_obj;
    
    std::vector<std::size_t> _A_i;
    std::vector<std::size_t> _A_j;
    std::vector<double> _A_x;
    std::vector<double> _obj;
    
    std::vector<double> _lb;
    std::vector<double> _ub;
    std::vector<double> _rhs;
    std::vector<std::string> _sense;
    std::vector<std::string> _vtype;
    
    std::vector<std::string> _row_ids;
    std::vector<std::string> _col_ids;
    
    // methods
    inline const std::size_t nrow() const {
      return(*std::max_element(_A_i.cbegin(), _A_i.cend()));
    }

    inline const std::size_t ncol() const {
      return(*std::max_element(_A_j.cbegin(), _A_j.cend()));
    }

    inline const std::size_t ncell() const {
      return(_A_x.size());
    }

    inline const Rcpp::List A() const {
      return(Rcpp::List::create(
        Rcpp::Named("i")=_A_i,
        Rcpp::Named("j")=_A_j,
        Rcpp::Named("x")=_A_x));
    }

};

#endif
