#pragma once
#ifndef FERRIER_SCORE_H
#define FERRIER_SCORE_H

#include "package.h"
#include "functions.h"

double calculate_feat_unit_irrep_value(
  double, double, double, double, double, double, double, double, double);

double calculate_standard_dev(double, double, double);

double calculate_adjusted_portfolio_size(double, double );

bool approx_equal(double, double);

double calculate_rx_removed(
  double, double, double, double, double, double, double);

double calculate_rx_included(
  double, double, double, double, double, double);

double calculate_rx_excluded(
  double, double, double, double, double, double, double);

double calculate_irr_feature(
  double, double, double, double, double, double);

double calculate_z_prob(double);

#endif
