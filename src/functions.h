#pragma once
#ifndef FUNCTIONS_H
#define FUNCTIONS_H

#include "package.h"

// power function, templated to use compile-time loop unrolling
template<int P>
inline double Pow(double x) {
  return (Pow<P-1>(x) * x);
}

template<>
inline double Pow<1>(double x) {
	return (x);
}

template<>
inline double Pow<0>(double x) {
  return (1.0);
}

// calculate euclidean distance
double distance(double, double, double, double);

// check if two numbers are approximately equal
bool approx_equal(double x, double y);

// convert object to string
template<typename T>
inline std::string num2str(T number, int precision=10) {
  std::ostringstream ss;
  ss << std::fixed << std::setprecision(precision) << number;
  return(ss.str());
}

void import_rij(Rcpp::List&, std::vector<arma::sp_mat>&);

void import_connectivity_matrix_list(Rcpp::List&,
                                     std::vector<std::vector<arma::sp_mat>>&,
                                     bool);

#endif
