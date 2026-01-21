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
```

## Arguments

- ...:

  not used.

## Details

The following functions have been deprecated:

- `add_connected_constraints()`:

  renamed as the
  [`add_contiguity_constraints()`](https://prioritizr.net/reference/add_contiguity_constraints.md)
  function.

- `add_corridor_constraints()`:

  replaced by the
  [`add_feature_contiguity_constraints()`](https://prioritizr.net/reference/add_feature_contiguity_constraints.md)
  function.

- `add_loglinear_targets()`:

  replaced by the
  [`spec_interp_absolute_targets()`](https://prioritizr.net/reference/spec_interp_absolute_targets.md)
  function.

- `set_number_of_threads()`:

  no longer needed due to improved data extraction methods.

- `get_number_of_threads()`:

  no longer needed due to improved data extraction methods.

- `is.parallel()`:

  no longer needed due to improved data extraction methods.

- `add_pool_portfolio()`:

  replaced by the
  [`add_extra_portfolio()`](https://prioritizr.net/reference/add_extra_portfolio.md)
  and
  [`add_top_portfolio()`](https://prioritizr.net/reference/add_top_portfolio.md).

- `connected_matrix()`:

  renamed as the
  [`adjacency_matrix()`](https://prioritizr.net/reference/adjacency_matrix.md)
  function.

- `feature_representation()`:

  replaced by the
  [`eval_feature_representation_summary()`](https://prioritizr.net/reference/eval_feature_representation_summary.md)
  function for consistency with other functions.

- `replacement_cost()`:

  renamed as the
  [`eval_replacement_importance()`](https://prioritizr.net/reference/eval_replacement_importance.md)
  function for consistency with other functions for evaluating
  solutions.

- `rarity_weighted_richness()`:

  renamed as the
  [`eval_rare_richness_importance()`](https://prioritizr.net/reference/eval_rare_richness_importance.md)
  function for consistency with other functions for evaluating
  solutions.

- `ferrier_score()`:

  renamed as the
  [`eval_ferrier_importance()`](https://prioritizr.net/reference/eval_ferrier_importance.md)
  function for consistency with other functions for evaluating
  solutions.

- `distribute_load()`:

  has been removed because it is no longer used. See
  [`parallel::splitIndices()`](https://rdrr.io/r/parallel/splitIndices.html)
  for equivalent functionality.

- `new_optimization_problem()`:

  replaced by
  [`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md).

- `predefined_optimization_problem()`:

  replaced by
  [`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md).
