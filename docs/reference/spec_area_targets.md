# Specify targets based on area units

Specify targets expressed as area-based units. For example, this
function can be used to express targets as hectares, acres, or km². To
ensure feasibility, area-based targets are clamped based on the total
abundance of features.

## Usage

``` r
spec_area_targets(targets, area_units)
```

## Arguments

- targets:

  `numeric` vector denoting the area-based targets for the features.
  These values must be expressed in the same units as `area_units`. If a
  single `numeric` value is specified, then all features are assigned
  targets based on the area-based target.

- area_units:

  `character` vector denoting the unit of measurement for each `targets`
  value (e.g., `"km^2"`, `"ha"`, and `"acres"`). If a single `character`
  value is specified, then all features are assigned targets assuming
  that `targets` are in the same units.

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

This method provides an approach for setting target thresholds based on
an area-based threshold. To express this mathematically, we will define
the following terminology. Let \\f\\ denote the total spatial extent of
a feature (e.g., geographic range size expressed as km²), and \\a\\ the
specified area-based target (expressed as km², per `targets` and
`area_units`). Given this terminology, the target threshold (\\t\\) for
the feature is calculated as follows. \$\$ t = min(f, a) \$\$

## Data calculations

This function involves calculating targets based on the spatial extent
of the features in `x`. Although it can be readily applied to
[`problem()`](https://prioritizr.net/reference/problem.md) objects that
have the feature data provided as a
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object, you will need to specify the spatial units for the features when
initializing the
[`problem()`](https://prioritizr.net/reference/problem.md) objects if
the feature data are provided in a different format. In particular, if
the feature data are provided as a `data.frame` or `character` vector,
then you will need to specify an argument to `feature_units` when using
the [`problem()`](https://prioritizr.net/reference/problem.md) function.
See the Examples section of the documentation for
[`add_auto_targets()`](https://prioritizr.net/reference/add_auto_targets.md)
for a demonstration of specifying the spatial units for features.

## See also

Other target setting methods:
[`spec_absolute_targets()`](https://prioritizr.net/reference/spec_absolute_targets.md),
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
sim_complex_pu_raster <- get_sim_complex_pu_raster()
sim_complex_features <- get_sim_complex_features()

# create base problem
p0 <-
  problem(sim_complex_pu_raster, sim_complex_features) %>%
  add_min_set_objective() %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# create problem with targets of 50 km^2 for each feature
p1 <-
  p0 %>%
  add_auto_targets(
    method = spec_area_targets(targets = 50, area_units = "km^2")
  )

# solve problem
s1 <- solve(p1)

# plot solution
plot(s1, main = "solution based on constant targets", axes = FALSE)


# targets can also be specified for each feature separately.
# to demonstrate this, we will set a target value for each
# feature based on a random number between 5000 and 30000 hectares
target_values <- runif(terra::nlyr(sim_complex_features), 5000, 30000)

# create problem with targets defined separately for each feature
p2 <-
  p0 %>%
  add_auto_targets(
    method = spec_area_targets(targets = target_values, area_units = "ha")
  )

# solve problem
s2 <- solve(p2)

# plot solution
plot(s2, main = "solution based on varying targets", axes = FALSE)

# }
```
