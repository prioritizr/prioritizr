# Specify relative targets

Specify targets expressed as a proportion (between 0 and 1) of the
maximum level of representation of each feature in the study area.
Please note that proportions are scaled according to the features' total
abundances in the study area (including any locked out planning units,
or planning units with `NA` cost values) using the
[`feature_abundances()`](https://prioritizr.net/reference/feature_abundances.md)
function. Note that this function is designed to be used with
[`add_auto_targets()`](https://prioritizr.net/reference/add_auto_targets.md)
and
[`add_group_targets()`](https://prioritizr.net/reference/add_group_targets.md).

## Usage

``` r
spec_relative_targets(targets, ...)
```

## Arguments

- targets:

  `numeric` vector that specifies targets for each of the features. If a
  single `numeric` value is specified, then all features are assigned
  the same proportion-based target. Note that values range between 0 and
  1 (corresponding to 0% and 100% respectively).

- ...:

  not used.

## Value

An object
([`TargetMethod`](https://prioritizr.net/reference/TargetMethod-class.md))
for specifying targets that can be used with
[`add_auto_targets()`](https://prioritizr.net/reference/add_auto_targets.md)
and
[`add_group_targets()`](https://prioritizr.net/reference/add_group_targets.md)
to add targets to a
[`problem()`](https://prioritizr.net/reference/problem.md).

## Mathematical formulation

This method involves setting target thresholds based on a proportion. To
express this mathematically, we will define the following terminology.
Let \\f\\ denote the total abundance of a feature (e.g., geographic
range size), and \\a\\ the relative target for the feature (per
`targets`). Given this terminology, the target threshold (\\t\\) for the
feature is calculated as follows. \$\$t = f \times a\$\$

## See also

To add relative targets directly to a
[`problem()`](https://prioritizr.net/reference/problem.md), see
[`add_relative_targets()`](https://prioritizr.net/reference/add_relative_targets.md).

Other target setting methods:
[`spec_absolute_targets()`](https://prioritizr.net/reference/spec_absolute_targets.md),
[`spec_area_targets()`](https://prioritizr.net/reference/spec_area_targets.md),
[`spec_duran_targets()`](https://prioritizr.net/reference/spec_duran_targets.md),
[`spec_interp_absolute_targets()`](https://prioritizr.net/reference/spec_interp_absolute_targets.md),
[`spec_interp_area_targets()`](https://prioritizr.net/reference/spec_interp_area_targets.md),
[`spec_jung_targets()`](https://prioritizr.net/reference/spec_jung_targets.md),
[`spec_max_targets()`](https://prioritizr.net/reference/spec_max_targets.md),
[`spec_min_targets()`](https://prioritizr.net/reference/spec_min_targets.md),
[`spec_polak_targets()`](https://prioritizr.net/reference/spec_polak_targets.md),
[`spec_pop_size_targets()`](https://prioritizr.net/reference/spec_pop_size_targets.md),
[`spec_rl_ecosystem_targets()`](https://prioritizr.net/reference/spec_rl_ecosystem_targets.md),
[`spec_rl_species_targets()`](https://prioritizr.net/reference/spec_rl_species_targets.md),
[`spec_rodrigues_targets()`](https://prioritizr.net/reference/spec_rodrigues_targets.md),
[`spec_rule_targets()`](https://prioritizr.net/reference/spec_rule_targets.md),
[`spec_ward_targets()`](https://prioritizr.net/reference/spec_ward_targets.md),
[`spec_watson_targets()`](https://prioritizr.net/reference/spec_watson_targets.md),
[`spec_wilson_targets()`](https://prioritizr.net/reference/spec_wilson_targets.md)

## Examples

``` r
# \dontrun{
# set seed for reproducibility
set.seed(500)

# load data
sim_complex_pu_raster <- get_sim_complex_pu_raster()
sim_complex_features <- get_sim_complex_features()

# create base problem
p0 <-
  problem(sim_complex_pu_raster, sim_complex_features) %>%
  add_min_set_objective() %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# create problem with targets of 10% for each feature
p1 <-
  p0 %>%
  add_auto_targets(method = spec_relative_targets(targets = 0.1))

# solve problem
s1 <- solve(p1)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.002 0.001 0.012

# plot solution
plot(s1, main = "solution based on 10% targets", axes = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 's1' not found

# targets can also be specified for each feature separately.
# to demonstrate this, we will set a target value for each
# feature based on a random percentage between 10% and 80%
target_values <- runif(terra::nlyr(sim_complex_features), 0.1, 0.8)

# create problem with targets defined separately for each feature
p2 <-
  p0 %>%
  add_auto_targets(method = spec_relative_targets(targets = target_values))

# solve problem
s2 <- solve(p2)
#> Error: Error 10009: HostID mismatch (licensed to 2890c3bc, hostid is d03f011a)
#> Timing stopped at: 0.003 0.001 0.014

# plot solution
plot(s2, main = "solution based on varying targets", axes = FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 's2' not found
# }
```
