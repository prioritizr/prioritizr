# Specify absolute targets

Specify targets expressed as the same values as the underlying feature
data (ignoring any specified feature units). For example, setting a
target of 10 for a feature specifies that a solution should ideally
select a set of planning units that contain a total (summed) value of,
at least, 10 for the feature. This function is designed to be used with
[`add_auto_targets()`](https://prioritizr.net/reference/add_auto_targets.md).

## Usage

``` r
spec_absolute_targets(targets, ...)
```

## Arguments

- targets:

  `numeric` vector that specifies targets for each of the features. If a
  single value is specified, then all features are assigned the same
  target threshold.

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

This method involves setting target thresholds based on a pre-specified
value. To express this mathematically, we will define the following
terminology. Let \\a\\ the absolute target for a feature (per
`targets`). Given this terminology, the target threshold (\\t\\) for the
feature is calculated as follows. \$\$t = a\$\$

## See also

To add relative targets directly to a
[`problem()`](https://prioritizr.net/reference/problem.md), see
[`add_absolute_targets()`](https://prioritizr.net/reference/add_absolute_targets.md).

Other target setting methods:
[`spec_area_targets()`](https://prioritizr.net/reference/spec_area_targets.md),
[`spec_duran_targets()`](https://prioritizr.net/reference/spec_duran_targets.md),
[`spec_interp_absolute_targets()`](https://prioritizr.net/reference/spec_interp_absolute_targets.md),
[`spec_interp_area_targets()`](https://prioritizr.net/reference/spec_interp_area_targets.md),
[`spec_jung_targets()`](https://prioritizr.net/reference/spec_jung_targets.md),
[`spec_max_targets()`](https://prioritizr.net/reference/spec_max_targets.md),
[`spec_min_targets()`](https://prioritizr.net/reference/spec_min_targets.md),
[`spec_polak_targets()`](https://prioritizr.net/reference/spec_polak_targets.md),
[`spec_pop_size_targets()`](https://prioritizr.net/reference/spec_pop_size_targets.md),
[`spec_relative_targets()`](https://prioritizr.net/reference/spec_relative_targets.md),
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
sim_pu_raster <- get_sim_pu_raster()
sim_features <- get_sim_features()

# create base problem
p0 <-
  problem(sim_pu_raster, sim_features) %>%
  add_min_set_objective() %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# this function sets targets based on the total abundance of the features
# (i.e., sum of planning unit values for the feature) and does not
# consider the spatial area covered by the planning units

# create problem with absolute targets of 5 for each feature
p1 <-
  p0 %>%
  add_auto_targets(method = spec_absolute_targets(targets = 5))

# solve problem
s1 <- solve(p1)

# plot solution
plot(s1, main = "solution based on constant targets", axes = FALSE)


# targets can also be specified for each feature separately.
# to demonstrate this, we will set a target value for each
# feature based on a random number between 1 and 5
target_values <- runif(terra::nlyr(sim_features), 1, 5)

# create problem with targets defined separately for each feature
p2 <-
  p0 %>%
  add_auto_targets(method = spec_absolute_targets(targets = target_values))

# solve problem
s2 <- solve(p2)

# plot solution
plot(s2, main = "solution based on varying targets", axes = FALSE)

# }
```
