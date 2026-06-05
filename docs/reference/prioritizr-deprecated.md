# Deprecation notice

The functions listed here are deprecated. This means that they once
existed in earlier versions of the of the prioritizr package, but they
have since been removed entirely, replaced by other functions, or
renamed as other functions in newer versions. To help make it easier to
transition to new versions of the prioritizr package, we have listed
alternatives for deprecated the functions (where applicable). If a
function is described as being renamed, then this means that only the
name of the function has changed (i.e., the inputs, outputs, and
underlying code remain the same).

## Usage

``` r
add_connected_constraints(...)

add_corridor_constraints(...)

set_number_of_threads(...)

get_number_of_threads(...)

is.parallel(...)

add_pool_portfolio(...)

connected_matrix(...)

feature_representation(...)

replacement_cost(...)

rarity_weighted_richness(...)

ferrier_score(...)

distribute_load(...)

new_optimization_problem(...)

predefined_optimization_problem(...)

add_loglinear_targets(...)

add_loglinear_targets(...)

add_max_phylo_objective(...)

add_max_utility_objective(...)
```

## Arguments

- ...:

  not used.

## Details

The following functions have been deprecated:

- `add_connected_constraints()`:

  Renamed as the
  [`add_contiguity_constraints()`](https://prioritizr.net/reference/add_contiguity_constraints.md)
  function.

- `add_corridor_constraints()`:

  Replaced by the
  [`add_feature_contiguity_constraints()`](https://prioritizr.net/reference/add_feature_contiguity_constraints.md)
  function.

- `add_loglinear_targets()`:

  Replaced by the
  [`spec_interp_absolute_targets()`](https://prioritizr.net/reference/spec_interp_absolute_targets.md)
  function.

- `add_max_features_objective()`:

  Renamed as the
  [`add_max_n_targets_met_objective()`](https://prioritizr.net/reference/add_max_n_targets_met_objective.md)
  function.

- `add_max_phylo_objective()`:

  Renamed as the
  [`add_max_phylo_div_objective()`](https://prioritizr.net/reference/add_max_phylo_div_objective.md)
  function.

- `add_max_utility_objective()`:

  Renamed as the
  [`add_max_wtd_sum_objective()`](https://prioritizr.net/reference/add_max_wtd_sum_objective.md)
  function.

- `set_number_of_threads()`:

  No longer needed due to improved data extraction methods.

- `get_number_of_threads()`:

  No longer needed due to improved data extraction methods.

- `is.parallel()`:

  No longer needed due to improved data extraction methods.

- `add_pool_portfolio()`:

  Replaced by the
  [`add_extra_portfolio()`](https://prioritizr.net/reference/add_extra_portfolio.md)
  and
  [`add_top_portfolio()`](https://prioritizr.net/reference/add_top_portfolio.md).

- `connected_matrix()`:

  Renamed as the
  [`adjacency_matrix()`](https://prioritizr.net/reference/adjacency_matrix.md)
  function.

- `feature_representation()`:

  Replaced by the
  [`eval_feature_representation_summary()`](https://prioritizr.net/reference/eval_feature_representation_summary.md)
  function for consistency with other functions.

- `replacement_cost()`:

  Renamed as the
  [`eval_replacement_importance()`](https://prioritizr.net/reference/eval_replacement_importance.md)
  function for consistency with other functions for evaluating
  solutions.

- `rarity_weighted_richness()`:

  Renamed as the
  [`eval_rare_richness_importance()`](https://prioritizr.net/reference/eval_rare_richness_importance.md)
  function for consistency with other functions for evaluating
  solutions.

- `ferrier_score()`:

  Renamed as the
  [`eval_ferrier_importance()`](https://prioritizr.net/reference/eval_ferrier_importance.md)
  function for consistency with other functions for evaluating
  solutions.

- `distribute_load()`:

  Removed because it is no longer used. See the
  [`parallel::splitIndices()`](https://rdrr.io/r/parallel/splitIndices.html)
  function for equivalent functionality.

- `new_optimization_problem()`:

  Replaced by the
  [`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md)
  function.

- `predefined_optimization_problem()`:

  Replaced by the
  [`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md)
  function.
