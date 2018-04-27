#ifndef RCPP_BOUNDARY_DATA_H
#define RCPP_BOUNDARY_DATA_H

#include "package.h"
#include <vector>
#include <array>
#include <string>
#include <algorithm>
#include <unordered_map>
#include <boost/functional/hash.hpp>
#include <boost/unordered_set.hpp>
#include <boost/unordered_map.hpp>
#include "functions.h"

inline bool is_between(double x0, double y0, double x1, double y1, double x2,
                double y2) {
  // code inspired by Cyrille Ka
  // https://stackoverflow.com/questions/328107/how-can-you-determine-a-point-is-between-two-other-points-on-a-line-segment

  // check if point 0 == point 1
  if ((fabs(x0 - x1) < 1.0e-10) & (fabs(y0 - y1) < 1.0e-10))
    return false;

  // check if point 0 == point 2
  if ((fabs(x0 - x2) < 1.0e-10) & (fabs(y0 - y2) < 1.0e-10))
    return false;

 // check if point 1 == point 2
  if ((fabs(x1 - x2) < 1.0e-10) & (fabs(y1 - y2) < 1.0e-10))
    return false;

  // check that first point is before second point
  double tmp;
  if (x0 > x1 || ((fabs(x0 - x1) < 1.0e-10) && (y1 < y0))) {
    // if points are in wrong order then swap them
    tmp = x0;
    x0 = x1;
    x1 = tmp;

    tmp = y0;
    y0 = y1;
    y1 = tmp;
  }

  // check if point 2 lays between points 0 and 1
  double crossproduct = (y2 - y0) * (x1 - x0) - (x2 - x0) * (y1 - y0);
  if (fabs(crossproduct) > 1.0e-10)
    return false;

  double dotproduct = (x2 - x0) * (x1 - x0) + (y2 - y0) * (y1 - y0);
  if (dotproduct < 0)
    return false;

  double squaredlengthba = (x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0);
  if (dotproduct > squaredlengthba)
    return false;

  return true;
}

class LINE
{
  typedef std::array<std::string, 4> LINEID;
  public:
  // declare constructor
  LINE(){};
  LINE(std::size_t pid, std::size_t pos0, std::size_t pos1, double x0,
        double y0, double x1, double y1, double tol)
    : _pid(pid), _pos0(pos0), _pos1(pos1) {
      if (x0 < x1 || ((x0 == x1) && (y0 < y1))) {
        _x0 = x0;
        _y0 = y0;
        _x1 = x1;
        _y1 = y1;
      } else {
        _x0 = x1;
        _y0 = y1;
        _x1 = x0;
        _y1 = y0;
      }
      _id = {{num2str<double>(_x0, tol), num2str<double>(_y0, tol),
              num2str<double>(_x1, tol), num2str<double>(_y1, tol)}};
  };
  // declare deconstructor
  ~LINE(){};

  // declare methods
  inline const double length() const {
    return(distance(_x0, _y0, _x1, _y1));
  }

  inline const std::string repr() const {
    return("invalid boundary data for planning unit: " +
           num2str<std::size_t>(_pid));
  }

  inline const std::string repr2() const {
    return("(" + _id[0] + ", " + _id[1] + ")(" + _id[2] + ", " + _id[3] + ")");
  }

  inline bool collinear_with(LINE y, std::vector<double> &pts_x,
                             std::vector<double> &pts_y) {
    // clear vectors
    pts_x.clear();
    pts_x.shrink_to_fit();
    pts_y.clear();
    pts_y.shrink_to_fit();

    // add starting point if is inside bounds
    if (is_between(_x0, _y0, _x1, _y1, y._x0, y._y0)) {
      Rcout << "adding start point" << std::endl;
      pts_x.push_back(y._x0);
      pts_y.push_back(y._y0);
    }

    // add end point if is inside bounds
    if (is_between(_x0, _y0, _x1, _y1, y._x1, y._y1)) {
      Rcout << "adding end point" << std::endl;
      pts_x.push_back(y._x1);
      pts_y.push_back(y._y1);
    }

    // return true if either points inside line
    return (pts_x .size() > 0);
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
