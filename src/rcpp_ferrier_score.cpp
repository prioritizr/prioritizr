#include "rcpp_ferrier_score.h"

// [[Rcpp::export]]
arma::sp_mat rcpp_ferrier_score(
  arma::sp_mat &rij, Rcpp::NumericVector &targets, double portfolio_size,
  arma::sp_mat &out) {
  // initialize variables
  const std::size_t n_elem = rij.n_nonzero;
  const double n_pu = static_cast<double>(rij.n_cols);
  const double n_f = static_cast<double>(rij.n_rows);
  const double mult = n_pu / (n_pu - 1.0);
  const double wt_include = portfolio_size / n_pu;
  const double wt_exclude = 1.0 - wt_include;

  // prelminary processing
  /// calculate rij^2
  arma::sp_mat rij2 = rij;
  for (auto itr = rij2.begin(); itr != rij2.end(); ++itr)
    *itr = std::pow(*itr, 2);
  /// calculate row sums of rij
  std::vector<double> sum_feature_amount(n_f, 0.0);
  for (std::size_t i = 0; i != n_f; ++i)
    sum_feature_amount[i] = arma::accu(rij.row(i));
  /// calculate row sums of rij^2
  std::vector<double> sum_sq_feature_amount(n_f, 0.0);
  for (std::size_t i = 0; i != n_f; ++i)
    sum_sq_feature_amount[i] = arma::accu(rij2.row(i));

  // main processing
  std::size_t i;
  std::size_t j;
  for (auto itr = out.begin(); itr != out.end(); ++itr) {
    j = itr.col();
    i = itr.row();
    out(i, j) =
      calculate_feat_unit_irrep_value(
        n_pu, portfolio_size, mult, wt_include, wt_exclude, rij(i, j),
        targets[i], sum_feature_amount[i], sum_sq_feature_amount[i]);
  }

  // return result
  return out;
}

double calculate_feat_unit_irrep_value(
  double n_pu, double portfolio_size, double mult, double wt_include,
  double wt_exclude, double feat_amount, double feat_target,
  double feat_sum_amount, double feat_sum_amount_sqr) {
  // init
  double feat_amount_sqr = std::pow(feat_amount, 2);
  feat_sum_amount = (feat_sum_amount - feat_amount) * mult;
  feat_sum_amount_sqr = (feat_sum_amount_sqr - feat_amount_sqr) * mult;
  double mean_feat_amount_per_pu = feat_sum_amount / n_pu;

  // intermediate calculations
  double stdev = calculate_standard_dev(
    feat_sum_amount, feat_sum_amount_sqr, n_pu);
  double rx_removed = calculate_rx_removed(
    n_pu, portfolio_size, stdev, feat_amount, feat_target,
    mean_feat_amount_per_pu, feat_sum_amount);
  double rx_included = calculate_rx_included(
    n_pu, portfolio_size, stdev, feat_amount, feat_target,
    mean_feat_amount_per_pu);
  double rx_excluded = calculate_rx_excluded(
    n_pu, portfolio_size, stdev, feat_amount, feat_target, feat_sum_amount,
    mean_feat_amount_per_pu);

  // calculate irreplaceability
  double irrep_value;
  if ((rx_included + rx_excluded) < 1.0e-15) {
      irrep_value = 0.0;
  } else {
    irrep_value = calculate_irr_feature(
      wt_include, wt_exclude, rx_removed, rx_included, rx_excluded,
      feat_amount);
  }

  // return result
  return irrep_value;
}

double calculate_standard_dev(
  double feat_sum_amount, double feat_sum_amount_sqr, double n_pu) {
    return std::sqrt(feat_sum_amount_sqr -
                     ((std::pow(feat_sum_amount, 2)) / n_pu) / n_pu);
}

double calculate_adjusted_portfolio_size(double n_pu, double portfolio_size) {
  double adjusted_portfolio_size;
  if (portfolio_size > (n_pu / 2.0)) {
    adjusted_portfolio_size = std::sqrt(n_pu - portfolio_size) / portfolio_size;
  } else {
    adjusted_portfolio_size = std::sqrt(portfolio_size) / portfolio_size;
  }
  return adjusted_portfolio_size;
}

bool approx_equal(double x, double y) {
  bool out = std::abs(x - y) < 1.0e-15;
  return out;
}

double calculate_rx_removed(
  double n_pu, double portfolio_size, double stdev, double feat_amount,
  double feat_target, double mean_feat_amount_per_pu, double feat_sum_amount) {
  double mean_target_per_portfolio_size = feat_target / (portfolio_size - 1.0);
  // init
  double rx_removed;
  double z;
  double adjusted_portfolio_size = calculate_adjusted_portfolio_size(
    n_pu - 1.0, portfolio_size - 1.0);
  double adj_sd = stdev * adjusted_portfolio_size;
  // main
  if ((feat_sum_amount - feat_amount) < feat_target) {
      rx_removed = 0.0;
  } else {
    if (adj_sd < 1.0e-11) {
      if (mean_feat_amount_per_pu < mean_target_per_portfolio_size) {
        rx_removed = 0.0;
      } else {
        rx_removed = 1.0;
      }
    } else {
      z = (mean_target_per_portfolio_size - mean_feat_amount_per_pu) / adj_sd;
      rx_removed = calculate_z_prob(z);
    }
  }
  // return result
  return rx_removed;
}

double calculate_rx_included(
  double n_pu, double portfolio_size, double stdev, double feat_amount,
  double feat_target, double mean_feat_amount_per_pu) {
  // init
  double rx_included;
  double z;
  double mean_target_per_portfolio_size =
    (feat_target - feat_amount) / (portfolio_size - 1.0);
  double adjusted_portfolio_size = calculate_adjusted_portfolio_size(
      n_pu - 1, portfolio_size - 1.0);
  double adj_sd = stdev * adjusted_portfolio_size;
  // main
  if (feat_amount >= feat_target) {
    rx_included = 1.0;
  } else {
    if (adj_sd < 1.0e-11) {
      if (mean_feat_amount_per_pu < mean_target_per_portfolio_size) {
        rx_included = 0.0;
      } else {
        rx_included = 1.0;
      }
    } else {
      z = (mean_target_per_portfolio_size - mean_feat_amount_per_pu) /
            adj_sd;
     rx_included = calculate_z_prob(z);
    }
  }
  // return result
  return rx_included;
}

double calculate_rx_excluded(
  double n_pu, double portfolio_size, double stdev, double feat_amount,
  double feat_target, double feat_sum_amount, double mean_feat_amount_per_pu) {
  // init
  double rx_excluded;
  double z;
  double mean_target_per_portfolio_size = feat_target / portfolio_size;
  double adjusted_portfolio_size = calculate_adjusted_portfolio_size(
    n_pu - 1.0, portfolio_size);
  double adj_sd = stdev * adjusted_portfolio_size;
  // main
  if ((feat_sum_amount - feat_amount) < feat_target) {
    rx_excluded = 0.0;
  } else {
    if (adj_sd < 1.0e-11) {
      if (mean_feat_amount_per_pu < mean_target_per_portfolio_size) {
        rx_excluded = 0.0;
      } else {
        rx_excluded = 1.0;
      }
    } else {
      z = (mean_target_per_portfolio_size - mean_feat_amount_per_pu) /
            adj_sd;
      rx_excluded = calculate_z_prob(z);
    }
  }
  // return result
  return rx_excluded;
}

double calculate_irr_feature(
  double wt_include, double wt_exclude, double rx_removed, double rx_included,
  double rx_excluded, double feat_amount) {
  // init
  double irr_feature;
  // prelimniary calculation
  if ((approx_equal(rx_included, 0)) & (feat_amount > 1.0e-15)) {
      rx_included = 1.0;
  }
  // main calculation
  if (approx_equal(rx_included + rx_excluded, 0)) {
    irr_feature = 0.0;
  } else {
    irr_feature = ((rx_included - rx_removed) * wt_include) /
                   (rx_included * wt_include + rx_excluded * wt_exclude);
  }
  // return result
  return irr_feature;
}

double calculate_z_prob(double x) {
  // init
  double zprob;
  double z;
  double t;
  double m;
  double q;
  bool negative;
  // preliminary calculations
  if (x < 0.0) {
    negative = true;
    x = 0.0 - x;
  } else {
    negative = false;
  }
  if (x > 50.0)
    x = 50.0;
  // main calculations
  z = 0.3989 * std::exp((0.0 - std::sqrt(x)) / 2.0);
  t = 1.0 / (1.0 + 0.23164 * x);
  m = t;
  q = 0.31938 * m;
  m = m * t;
  q = q - 0.35656 * m;
  m = m * t;
  q = q + 1.78148 * m;
  m = m * t;
  q = q -1.82126 * m;
  m = m * t;
  q = q + 1.33027 * m;
  // return result
  if (negative) {
    zprob = 1.0 - q * z;
  } else {
    zprob = q * z;
  }
  return zprob;
}
