#include "package.h"
#include "optimization_problem.h"

// [[Rcpp::export]]
Rcpp::List rcpp_apply_hierarchical_approach(const Rcpp::List x) {
  // Initialization
  Rcpp::print(Rcpp::wrap("here1"));
  /// define counters
  const std::size_t n = x.size();
  /// import optimization problems
  std::vector<Rcpp::XPtr<OPTIMIZATIONPROBLEM>> opt;
  opt.reserve(n);
  for (std::size_t i = 0; i < n; ++i) {
    opt.push_back(Rcpp::as<Rcpp::XPtr<OPTIMIZATIONPROBLEM>>(x[i]));
  }
  // define additional counters for optimization problems
  const std::size_t n_pu = opt[0]->_number_of_planning_units;
  const std::size_t n_zone = opt[0]->_number_of_zones;
  const std::size_t n_status = n_pu * n_zone;
  // define counters to store object sizes
  std::vector<std::size_t> opt_n_ncol(n);
  std::vector<std::size_t> opt_n_nrow(n);
  std::vector<std::size_t> opt_n_A(n);
  std::vector<std::size_t> opt_n_features(n);
  for (std::size_t i = 0; i < n; ++i) {
    opt_n_ncol[i] = opt[i]->ncol();
    opt_n_nrow[i] = opt[i]->nrow();
    opt_n_A[i] = opt[i]->_A_i.size();
    opt_n_features[i] = opt[i]->_number_of_features;
  }

  // define offset variables for rows and columns
  std::vector<std::size_t> opt_row_offset(n, 0);
  for (std::size_t i = 1; i < n; ++i) {
    opt_row_offset[i] = opt_n_nrow[i - 1];
  }
  for (std::size_t i = 1; i < n; ++i) {
    opt_row_offset[i] = opt_row_offset[i] + opt_row_offset[i - 1];
  }
  std::vector<std::size_t> opt_col_offset(n, 0);
  for (std::size_t i = 1; i < n; ++i) {
    opt_col_offset[i] = opt_n_ncol[i - 1] - n_status;
  }
  for (std::size_t i = 1; i < n; ++i) {
    opt_col_offset[i] = opt_col_offset[i] + opt_col_offset[i - 1];
  }
  std::vector<std::size_t> opt_A_offset(n, 0);
  for (std::size_t i = 1; i < n; ++i) {
    opt_A_offset[i] = opt_n_A[i - 1];
  }
  for (std::size_t i = 1; i < n; ++i) {
    opt_A_offset[i] = opt_A_offset[i] + opt_A_offset[i - 1];
  }

  // compute dimensions for multi-objective problem
  const std::size_t mopt_ncol =
    std::accumulate(opt_n_ncol.begin(), opt_n_ncol.end(), 0) -
    ((n - 1) * n_status);
  const std::size_t mopt_nrow =
    std::accumulate(opt_n_nrow.begin(), opt_n_nrow.end(), 0);
  const std::size_t mopt_n_A =
    std::accumulate(opt_n_A.begin(), opt_n_A.end(), 0);

  // initialize new optimization problem
  OPTIMIZATIONPROBLEM* mopt = new OPTIMIZATIONPROBLEM(
    std::string("min"),                   // modelsense
    std::accumulate(                      // number_of_features
      opt_n_features.begin(),
      opt_n_features.end(), 0
    ),
    n_pu,                                 // number_of_planning_units
    n_zone,                               // number_of_zones
    std::vector<std::size_t>(mopt_n_A),   // A_i
    std::vector<std::size_t>(mopt_n_A),   // A_j
    std::vector<double>(mopt_n_A),        // A_x
    std::vector<double>(mopt_ncol, 0.0),  // obj
    std::vector<double>(mopt_ncol),       // lb
    std::vector<double>(mopt_ncol),       // ub
    std::vector<double>(mopt_nrow),       // rhs
    std::vector<std::string>(mopt_nrow),  // sense
    std::vector<std::string>(mopt_ncol),  // vtype
    std::vector<std::string>(mopt_nrow),  // row_ids
    std::vector<std::string>(mopt_ncol),  // col_ids
    true                                  // compressed_formulation
  );

  // prepare multi-objective obj matrix
  Rcpp::NumericMatrix mobj(n, mopt_ncol);
  for (std::size_t i = 0; i < n; ++i) {
    /// store values for planning unit status variables
    for (std::size_t j = 0; j < n_status; ++j) {
      mobj(i, j) += opt[i]->_obj[j];
    }
    /// store values for extra variables
    for (std::size_t j = n_status; j < opt_n_ncol[i]; ++j) {
      mobj(i, j + opt_col_offset[i]) = opt[i]->_obj[j];
    }
  }

  // prepare multi-objective model sense
  Rcpp::CharacterVector mmodelsense(n);
  for (std::size_t i = 0; i < n; ++i) {
    mmodelsense[i] = Rcpp::wrap(opt[i]->_modelsense);
  }

  Rcpp::print(Rcpp::wrap("here3"));
  // specify lower bounds for multi-objective problem
  /// store values for planning unit status variables based on 1st problem
  /// and also store extra variables for 1st problem
  std::copy(
    opt[0]->_lb.begin(),
    opt[0]->_lb.end(),
    mopt->_lb.begin()
  );
  /// store values for extra variables for subsequent problems
  for (std::size_t i = 1; i < n; ++i) {
    std::copy(
      opt[i]->_lb.begin() + n_status,
      opt[i]->_lb.end(),
      mopt->_lb.begin() + n_status + opt_col_offset[i]
    );
  }

  // specify upper bounds for multi-objective problem
  /// store values for planning unit status variables based on 1st problem
  /// and also store extra variables for 1st problem
  std::copy(
    opt[0]->_ub.begin(),
    opt[0]->_ub.end(),
    mopt->_ub.begin()
  );
  /// store values for extra variables for subsequent problems
  for (std::size_t i = 1; i < n; ++i) {
    std::copy(
      opt[i]->_ub.begin() + n_status,
      opt[i]->_ub.end(),
      mopt->_ub.begin() + n_status + opt_col_offset[i]
    );
  }

  // specify variable types for multi-objective problem
  /// store values for planning unit status variables based on 1st problem
  /// and also store extra variables for 1st problem
  std::copy(
    opt[0]->_vtype.begin(),
    opt[0]->_vtype.end(),
    mopt->_vtype.begin()
  );
  /// store values for extra variables for subsequent problems
  for (std::size_t i = 1; i < n; ++i) {
    if (opt_n_ncol[i] > n_status) {
      std::copy(
        opt[i]->_vtype.begin() + n_status,
        opt[i]->_vtype.end(),
        mopt->_vtype.begin() + n_status + opt_col_offset[i]
      );
    }
  }

  Rcpp::print(Rcpp::wrap("here6"));
  // specify col ids for multi-objective problem
  /// store values for planning unit status variables based on 1st problem
  /// and also store extra variables for 1st problem
  std::copy(
    opt[0]->_col_ids.begin(),
    opt[0]->_col_ids.end(),
    mopt->_col_ids.begin()
  );
  /// store values for extra variables for subsequent problems
  for (std::size_t i = 1; i < n; ++i) {
    if (opt_n_ncol[i] > n_status) {
      std::copy(
        opt[i]->_col_ids.begin() + n_status,
        opt[i]->_col_ids.end(),
        mopt->_col_ids.begin() + n_status + opt_col_offset[i]
      );
    }
  }

  Rcpp::print(Rcpp::wrap("here7"));
  // store rhs for multi-objective problem
  /// store values for all problems
  for (std::size_t i = 0; i < n; ++i) {
    std::copy_n(
      opt[i]->_rhs.begin(),
      opt[i]->_rhs.size(),
      mopt->_rhs.begin() + opt_row_offset[i]
    );
  }

  Rcpp::print(Rcpp::wrap("here8"));
  // store sense for multi-objective problem
  /// store values for all problems
  for (std::size_t i = 0; i < n; ++i) {
    std::copy_n(
      opt[i]->_sense.begin(),
      opt[i]->_sense.size(),
      mopt->_sense.begin() + opt_row_offset[i]
    );
  }

  Rcpp::print(Rcpp::wrap("here9"));
  // store row_ids for multi-objective problem
  /// store values for all problems
  for (std::size_t i = 0; i < n; ++i) {
    std::copy_n(
      opt[i]->_row_ids.begin(),
      opt[i]->_row_ids.size(),
      mopt->_row_ids.begin() + opt_row_offset[i]
    );
  }

  Rcpp::print(Rcpp::wrap("here10"));
  // store A_i for multi-objective problem
  for (std::size_t i = 0; i < n; ++i) {
    for (std::size_t j = 0; j < opt[i]->_A_i.size(); ++j) {
      mopt->_A_i[j + opt_A_offset[i]] =
        opt[i]->_A_i[j] + opt_row_offset[i];
    }
  }

  Rcpp::print(Rcpp::wrap("here11"));
  // store A_x for multi-objective problem
  for (std::size_t i = 0; i < n; ++i) {
    for (std::size_t j = 0; j < opt[i]->_A_x.size(); ++j) {
      mopt->_A_x[j + opt_A_offset[i]] = opt[i]->_A_x[j];
    }
  }

  Rcpp::print(Rcpp::wrap("here12"));
  // store A_j for multi-objective problem
  double curr_offset;
  for (std::size_t i = 0; i < n; ++i) {
    /// store values for planning unit status variables
    for (std::size_t j = 0; j < opt[i]->_A_j.size(); ++j) {
      /// calculate offset for j'th value:
      /// if j'th value is a planning unit status variable, then we
      /// do not want to apply the offset and so multiply by 0
      /// otherwise, then we do want to appy the offset and so multiply by 1
      curr_offset =
        opt_col_offset[i] *
        static_cast<double>(opt[i]->_A_j[j] > n_status);
      mopt->_A_j[j + opt_A_offset[i]] = opt[i]->_A_j[j] + curr_offset;
    }
  }

  Rcpp::print(Rcpp::wrap("here13"));

  // prepare pointer for multi-objective optimization problem
  Rcpp::XPtr<OPTIMIZATIONPROBLEM> ptr =
    Rcpp::XPtr<OPTIMIZATIONPROBLEM>(mopt, true);

  // prepare output
  Rcpp::List out = List::create(
    Rcpp::Named("mopt") = ptr,
    Rcpp::Named("obj") = mobj,
    Rcpp::Named("modelsense") = mmodelsense
  );
  return(out);
}
