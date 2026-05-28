# Specify targets based on interpolating area-based thresholds

Specify targets by interpolating them between area-based thresholds.
Briefly, this method involves (i) setting target thresholds for rare
features to a particular percentage threshold, (ii) setting target
thresholds for common features to a particular percentage threshold, and
(iii) interpolating target thresholds for features with spatial
distributions that range between the those for the rare and common
features. Additionally, features can (optionally) have their targets
capped at a particular threshold. This method is especially useful for
setting targets based on interpolation procedures when features have
data expressed as an area-based unit of measurement (e.g., km²). Note
that this function is designed to be used with
[`add_auto_targets()`](https://prioritizr.net/reference/add_auto_targets.md)
and
[`add_group_targets()`](https://prioritizr.net/reference/add_group_targets.md).

## Usage

``` r
spec_interp_area_targets(
  rare_area_threshold,
  rare_relative_target,
  rare_area_target,
  rare_method,
  common_area_threshold,
  common_relative_target,
  common_area_target,
  common_method,
  cap_area_target,
  interp_method,
  area_units
)
```

## Arguments

- rare_area_threshold:

  `numeric` value indicating the threshold area for identifying rare
  features. This value must be expressed in the same units as the
  feature data. In particular, features with a total spatial extent
  smaller than this value will be considered rare during the target
  setting calculations.

- rare_relative_target:

  `numeric` value indicating the relative target for rare features. Note
  that this value must be a proportion between 0 and 1. For example, a
  value of 0.1 corresponds to 10%.

- rare_area_target:

  `numeric` value denoting the area-based target for rare features. This
  value must be expressed in the same units as `area_units`. To avoid
  setting an area-based target for rare features, a missing (`NA`) value
  can be specified.

- rare_method:

  `character` value indicating how the target for rare features should
  be calculated. Available options include `"min"` and `"max"`. For
  example, a value of `"max"` means that the target for a rare features
  is calculated as the maximum based on `rare_relative_target` and
  `rare_area_threshold`. Note that `rare_method` will have no effect on
  the target calculations if `rare_area_target` is a missing (`NA`)
  value.

- common_area_threshold:

  `numeric` value indicating the threshold area for identifying common
  features. This value must be expressed in the same units as
  `area_units`. In particular, features with a total spatial extent
  greater than this value will be considered common during the target
  setting calculations.

- common_relative_target:

  `numeric` value denoting the relative target for common features. Note
  that this value must be a proportion between 0 and 1. For example, a
  value of 0.1 corresponds to 10%.

- common_area_target:

  `numeric` value denoting the area-based target for common features.
  This value must be expressed in the same units as `area_units`. To
  avoid setting an area-based target for common features, a missing
  (`NA`) value can be specified.

- common_method:

  `character` value indicating how the target for common features should
  be calculated. Available options include `"min"` and `"max"`. For
  example, a value of `"max"` means that the target for a common feature
  is calculated as the maximum based on `common_relative_target` and
  `common_area_threshold`. Note that `common_method` will have no effect
  on the target calculations if `common_area_target` is a missing (`NA`)
  value.

- cap_area_target:

  `numeric` value denoting the area-based target cap. This value must be
  expressed in the same units as `area_units`. In particular, all
  targets are clamped to this value during target setting calculations.
  To avoid setting a target cap, a missing (`NA`) value can be
  specified.

- interp_method:

  `character` value denoting the interpolation method. Available options
  include `"linear"` for linear interpolation and `"log10"` for
  log-linear interpolation.

- area_units:

  `character` value denoting the unit of measurement for the area-based
  arguments (e.g., `"km^2", `"ha"`, `"acres"\`).

## Value

An object
([`TargetMethod`](https://prioritizr.net/reference/TargetMethod-class.md))
for specifying targets that can be used with
[`add_auto_targets()`](https://prioritizr.net/reference/add_auto_targets.md)
and
[`add_group_targets()`](https://prioritizr.net/reference/add_group_targets.md)
to add targets to a
[`problem()`](https://prioritizr.net/reference/problem.md).

## Details

This method has been applied to set target thresholds at global and
national scales (e.g., Butchart *et al.* 2015; Rodrigues *et al.* 2004;
Polak *et al.* 2015). It is based on the rationale that species with a
smaller geographic distribution are at a greater risk of extinction, and
so require a larger percentage of their geographic distribution to be
represented by a prioritization (Rodrigues *et al.* 2004). When using
this method in a planning exercise, it is important to ensure that the
threshold parameters reflect the stakeholder objectives. Additionally,
the threshold parameters may need to set according to the spatial extent
of the planning region.

## Mathematical formulation

This method provides a flexible approach for setting target thresholds
based on an interpolation procedure and the spatial extent of the
features. To express this mathematically, we will define the following
terminology. Let \\f\\ denote the total spatial extent of a feature
(e.g., geographic range size), \\a\\ the threshold for identifying rare
features (per `rare_area_threshold` and `area_units`), \\b\\ the
relative targets for rare features (per `rare_relative_target`), \\c\\
the area-based targets for rare features (per `rare_area_target` and
`area_units`), \\d()\\ the function for calculating targets for rare
features as a maximum or minimum value (per `rare_method`), \\e\\ the
threshold for identifying common features (per `common_area_threshold`
and `area_units`), \\g\\ the relative targets for common features (per
`common_relative_target`), \\h\\ the area-based targets for common
features (per `common_area_target` and `area_units`), \\i()\\ the method
for calculating targets for common features as a maximum or minimum
value (per `common_method`), and \\j\\ the target cap (per
`cap_area_target` and `area_units`), and \\k()\\ the interpolation
method for features with a spatial distribution that is larger than a
rare features and smaller than a common feature (per `interp_method`).
In particular, \\k()\\ is either a linear or log-linear interpolation
procedure based on the thresholds for identifying rare and common
features as well as the relative targets for rare and common features.
Given this terminology, the target threshold (\\t\\) for the feature is
calculated as follows.

- If \\f \< a\\, then \\ t = min(d(c, b \times f), j)\\.

- If \\f \> e\\, then \\ t = min(i(h, g \times f), j)\\.

- If \\a \leq f \leq e\\, then \\t = min(k(f, a, b, e, g), j)\\.

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
then you will need to specify `feature_units` when using the
[`problem()`](https://prioritizr.net/reference/problem.md) function. See
the Examples section of the documentation for
[`add_auto_targets()`](https://prioritizr.net/reference/add_auto_targets.md)
for a demonstration of specifying the spatial units for features.

## References

Butchart SHM, Clarke M, Smith RJ, Sykes RE, Scharlemann JPW, Harfoot M,
Buchanan GM, Angulo A, Balmford A, Bertzky B, Brooks TM, Carpenter KE,
Comeros‐Raynal MT, Cornell J, Ficetola GF, Fishpool LDC, Fuller RA,
Geldmann J, Harwell H, Hilton‐Taylor C, Hoffmann M, Joolia A, Joppa L,
Kingston N, May I, Milam A, Polidoro B, Ralph G, Richman N, Rondinini C,
Segan DB, Skolnik B, Spalding MD, Stuart SN, Symes A, Taylor J, Visconti
P, Watson JEM, Wood L, Burgess ND (2015) Shortfalls and solutions for
meeting national and global conservation area targets. *Conservation
Letters*, 8: 329–337.

Polak T, Watson JEM, Fuller RA, Joseph LN, Martin TG, Possingham HP,
Venter O, Carwardine J (2015) Efficient expansion of global protected
areas requires simultaneous planning for species and ecosystems. *Royal
Society Open Science*, 2: 150107.

Rodrigues ASL, Akçakaya HR, Andelman SJ, Bakarr MI, Boitani L, Brooks
TM, Chanson JS, Fishpool LDC, Da Fonseca GAB, Gaston KJ, Hoffmann M,
Marquet PA, Pilgrim JD, Pressey RL, Schipper J, Sechrest W, Stuart SN,
Underhill LG, Waller RW, Watts MEJ, Yan X (2004) Global gap analysis:
priority regions for expanding the global protected-area network.
*BioScience*, 54: 1092–1100.

## See also

Other target setting methods:
[`spec_absolute_targets()`](https://prioritizr.net/reference/spec_absolute_targets.md),
[`spec_area_targets()`](https://prioritizr.net/reference/spec_area_targets.md),
[`spec_duran_targets()`](https://prioritizr.net/reference/spec_duran_targets.md),
[`spec_interp_absolute_targets()`](https://prioritizr.net/reference/spec_interp_absolute_targets.md),
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

# create problem with interpolated targets.
# here, targets will be set as 100% for features smaller than 1000 km^2
# in size, 10% for features greater than 250,000 km^2 in size,
# log-linearly interpolated for features with an intermediate range size,
# and capped at 1,000,000 km^2
p1 <-
  problem(sim_complex_pu_raster, sim_complex_features) %>%
  add_min_set_objective() %>%
  add_auto_targets(
    method = spec_interp_area_targets(
     rare_area_threshold = 1000,
     rare_relative_target = 1,
     rare_area_target = NA,            # not used
     rare_method = "max",              # not used
     common_area_threshold = 250000,
     common_relative_target = 0.1,
     common_area_target = NA,          # not used
     common_method = "max",            # not used
     cap_area_target = 1000000,
     interp_method = "log10",
     area_units = "km^2"
    )
  ) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# solve problem
s1 <- solve(p1)

# plot solution
plot(s1, main = "solution", axes = FALSE)

# }
```
