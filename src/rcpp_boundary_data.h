#ifndef RCPP_BOUNDARY_DATA_H
#define RCPP_BOUNDARY_DATA_H

#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

#include <vector>
#include <string>
#include <algorithm>
#include <unordered_map>
#include <boost/functional/hash.hpp>
#include <boost/unordered_set.hpp>

#include "functions.h"

class LINE
{
  typedef std::array<int,4> LINEID;
  public:
  // declare constructor
  LINE(){};
  LINE(std::size_t pid, std::size_t pos0, std::size_t pos1, double x0,
        double y0, double x1, double y1, double tol) 
    : _pid(pid), _pos0(pos0), _pos1(pos1), _x0(x0), _y0(y0), _x1(x1), _y1(y1) {            
    if (_x0 > _x1 || (_x0 == _x1 && _y0 > _y1)) {
      _id = {{static_cast<int>(_x0 * tol), static_cast<int>(_y0 * tol),
              static_cast<int>(_x1 * tol), static_cast<int>(_y1 * tol)}};
    } else {
      _id = {{static_cast<int>(_x1 * tol), static_cast<int>(_y1 * tol),
              static_cast<int>(_x0 * tol), static_cast<int>(_y0 * tol)}};
    }
  };
  // declare deconstructor
  ~LINE(){};

  // declare methods
  inline const double length() const {
    return(distance(_x0,_y0,_x1,_y1));
  }
  
  inline const std::string repr() const {
    return("invalid boundary data for planning unit: " + num2str<std::size_t>(_pid));
  }
  
  // declare fields
  std::size_t _pid;
  std::size_t _pos0;
  std::size_t _pos1;
  double _x0;
  double _y0;
  double _x1;
  double _y1;
  LINEID _id;
};
      
class PUPAIR
{
  typedef std::pair<std::size_t,std::size_t> PUPAIRID;
  public:
  // declare constructor
  PUPAIR(){};
  PUPAIR(std::size_t pid0, std::size_t pid1, double length)
    : _pid0(pid0), _pid1(pid1), _length(length) {
      if (_pid0 > _pid1) {
        _id = PUPAIRID(_pid0, _pid1);
      } else {
        _id = PUPAIRID(_pid1, _pid0);
      }
  };

  // declare deconstructor
  ~PUPAIR(){};

  // declare fields
  int _pid0;
  int _pid1;
  double _length;
  PUPAIRID _id;
};


#endif

 
