#include "rcpp_ferrier_score.h"

// [[Rcpp::export]]
arma::sp_mat rcpp_ferrier_score(
  arma::sp_mat &rij, Rcpp::NumericVector &targets, double portfolio_size,
  arma::sp_mat &out) {
  // initialize variables
  const double n_pu = static_cast<double>(rij.n_cols);
  const double n_f = static_cast<double>(rij.n_rows);
  const double mult = n_pu / (n_pu - 1.0);
  const double wt_include = portfolio_size / n_pu;
  const double wt_exclude = 1.0 - wt_include;

  // prelminary processing
  /// calculate rij^2
  arma::sp_mat rij2 = rij;
  for (auto itr = rij2.begin(); itr != rij2.end(); ++itr)
    *itr = Pow<2>(*itr);
  /// calculate row sums of rij
  std::vector<double> sum_feat_amount(n_f, 0.0);
  for (std::size_t i = 0; i != n_f; ++i)
    sum_feat_amount[i] = arma::accu(rij.row(i));
  /// calculate row sums of rij^2
  std::vector<double> sum_sq_feat_amount(n_f, 0.0);
  for (std::size_t i = 0; i != n_f; ++i)
    sum_sq_feat_amount[i] = arma::accu(rij2.row(i));

  // main processing
  std::size_t i;
  std::size_t j;
  for (auto itr = out.begin(); itr != out.end(); ++itr) {
    j = itr.col();
    i = itr.row();
    out(i, j) =
      calculate_feat_unit_irrep_value(
        n_pu, portfolio_size, mult, wt_include, wt_exclude, rij(i, j),
        targets[i], sum_feat_amount[i], sum_sq_feat_amount[i]);
  }

  // return result
  return out;
}

double calculate_feat_unit_irrep_value(
  double n_pu,
  double portfolio_size,
  double mult,
  double wt_include,
  double wt_exclude,
  double feat_amount,
  double feat_target,
  double sum_feat_amount,
  double sum_sq_feat_amount) {
  // init
  double feat_amount_sq = Pow<2>(feat_amount);
  sum_feat_amount = (sum_feat_amount - feat_amount) * mult;
  sum_sq_feat_amount = (sum_sq_feat_amount - feat_amount_sq) * mult;
  double mean_feat_amount_per_pu = sum_feat_amount / n_pu;

  // intermediate calculations
  double stdev = calculate_standard_dev(
    sum_feat_amount, sum_sq_feat_amount, n_pu);

  double rx_removed = calculate_rx_removed(
    n_pu, portfolio_size, stdev, feat_amount,
    feat_target, mean_feat_amount_per_pu, sum_feat_amount);

  double rx_included = calculate_rx_included(
    n_pu, portfolio_size, stdev, feat_amount,
    feat_target, mean_feat_amount_per_pu);

  double rx_excluded = calculate_rx_excluded(
    n_pu, portfolio_size, stdev, feat_amount,
    feat_target, sum_feat_amount, mean_feat_amount_per_pu);

  // calculate the irreplaceability value
  double irrep_value;
  if (approx_equal(rx_included + rx_excluded, 0.0)) {
    irrep_value = 0.0;
  } else {
    if (approx_equal(rx_included, 0.0) & (feat_amount > 1.0e-15)) {
      rx_included = 1.0;
    }
    if (approx_equal(rx_included + rx_excluded, 0.0)) {
      irrep_value = 0.0;
    } else {
      irrep_value = ((rx_included - rx_removed) * wt_include) /
        (rx_included * wt_include + rx_excluded * wt_exclude);
    }
  }

  // return result
  return irrep_value;
}

double calculate_standard_dev(
  double sum_feat_amount,
  double sum_sq_feat_amount,
  double n_pu) {
  return std::sqrt(
    sum_sq_feat_amount - ((Pow<2>(sum_feat_amount)) / n_pu) / n_pu
  );
}

double calculate_rx_removed(
  double n_pu,
  double portfolio_size,
  double stdev,
  double feat_amount,
  double feat_target,
  double mean_feat_amount_per_pu,
  double sum_feat_amount) {
  // init
  double rx_removed;
  Rcpp::NumericVector z(1);
  double mean_target_per_portfolio_size =
    feat_target / (portfolio_size - 1.0);
  double adj_sd = stdev * calculate_adjusted_portfolio_size(
    n_pu - 1.0, portfolio_size - 1.0);
  // main
  if ((sum_feat_amount - feat_amount) < feat_target) {
    rx_removed = 0.0;
  } else {
    if (adj_sd < 1.0e-11) {
      if (mean_feat_amount_per_pu < mean_target_per_portfolio_size) {
        rx_removed = 0.0;
      } else {
        rx_removed = 1.0;
      }
    } else {
      z[0] = (mean_target_per_portfolio_size - mean_feat_amount_per_pu) /
           adj_sd;
      rx_removed = 1.0 - Rcpp::pnorm(z)[0]; // area under the right tail
    }
  }
  // return result
  return rx_removed;
}

double calculate_rx_included(
  double n_pu,
  double portfolio_size,
  double stdev,
  double feat_amount,
  double feat_target,
  double mean_feat_amount_per_pu) {
  // init
  double rx_included;
  Rcpp::NumericVector z(1);
  double mean_target_per_portfolio_size =
    (feat_target - feat_amount) / (portfolio_size - 1.0);
  double adj_sd = stdev * calculate_adjusted_portfolio_size(
    n_pu - 1.0, portfolio_size - 1.0);
  // main
  if (feat_amount >= feat_target) {
    rx_included = 1.0;
  } else {
    if (adj_sd < 1.0e-11) {
      if (mean_feat_amount_per_pu < mean_target_per_portfolio_size) {
        rx_included =  0.0;
      } else {
        rx_included = 1.0;
      }
    } else {
      z[0] = (mean_target_per_portfolio_size - mean_feat_amount_per_pu) /
           adj_sd;
      rx_included = 1.0 - Rcpp::pnorm(z)[0]; // area on the right tail
    }
  }
  // return result
  return rx_included;
}

double calculate_rx_excluded(
  double n_pu,
  double portfolio_size,
  double stdev,
  double feat_amount,
  double feat_target,
  double sum_feat_amount,
  double mean_feat_amount_per_pu) {
  // init
  double rx_excluded ;
  Rcpp::NumericVector z(1);
  double mean_target_per_portfolio_size = feat_target / portfolio_size;
  double adj_sd = stdev * calculate_adjusted_portfolio_size(
    n_pu - 1, portfolio_size);
  // main
  if ((sum_feat_amount - feat_amount) < feat_target) {
    rx_excluded = 0;
  } else {
    if (adj_sd < 1.0e-11) {
      if (mean_feat_amount_per_pu < mean_target_per_portfolio_size) {
        rx_excluded = 0.0;
      } else {
        rx_excluded = 1.0;
      }
    } else {
      z[0] = (mean_target_per_portfolio_size - mean_feat_amount_per_pu) /
              adj_sd;
      rx_excluded = 1.0 - Rcpp::pnorm(z)[0]; // area under the right tail
    }
  }
  // return result
  return rx_excluded;
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
