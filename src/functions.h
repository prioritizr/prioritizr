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

// check if two numbers are approximately equal
bool approx_equal(double x, double y);

// convert object to string
template<typename T>
inline std::string num2str(T number, int precision=10) {
  std::ostringstream ss;
  ss << std::fixed << std::setprecision(precision) << number;
  return(ss.str());
}

void import_rij(const Rcpp::List&, std::vector<arma::sp_mat>&);

void import_connectivity_matrix_list(
  const Rcpp::List&,
  std::vector<std::vector<arma::sp_mat>>&,
  bool
);

// wrapper around R's RNG so that users can use set.seed() in R
// console to make analyses reproducible
// obtained from here: http://gallery.rcpp.org/articles/stl-random-shuffle/
inline int rand_wrapper(const int n) { return floor(unif_rand()*n); }

void r_random_shuffle(std::vector<std::size_t>&);

#endif
