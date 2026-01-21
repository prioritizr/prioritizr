# Specify targets following Polak *et al.* (2015)

Specify targets based on the methodology outlined by Polak *et al.*
(2015). Briefly, this method involves setting targets based on linear
interpolation methods. To help prevent widespread features from
obscuring priorities, targets are capped following Butchart *et al.*
(2015). Note that this function is designed to be used with
[`add_auto_targets()`](https://prioritizr.net/reference/add_auto_targets.md)
and
[`add_group_targets()`](https://prioritizr.net/reference/add_group_targets.md).

## Usage

``` r
spec_polak_targets(
  rare_area_threshold = 1000,
  rare_relative_target = 1,
  common_area_threshold = 10000,
  common_relative_target = 0.1,
  cap_area_target = 1e+06,
  area_units = "km^2"
)
```

## Arguments

- rare_area_threshold:

  `numeric` value indicating the threshold area for rare identifying
  rare features. Defaults to 1000 (i.e., 1000 km²).

- rare_relative_target:

  `numeric` value denoting the relative target for features with a
  spatial distribution that is smaller than `rare_area_threshold`. Note
  that this value must be a proportion between 0 and 1. Defaults to 1
  (i.e., 100%).

- common_area_threshold:

  `numeric` value indicating the threshold area for identifying common
  features. Defaults to 10000 (i.e., 10,000 km²).

- common_relative_target:

  `numeric` value denoting the relative target for features with a
  spatial distribution that is greater than `common_area_threshold`.
  Defaults to 0.1 (i.e., 10%). Since this default value is based on
  historical levels of global protected area coverage, it may be
  appropriate to set this value based on current levels of protected
  area coverage (e.g., 17.6% for terrestrial and 8.4% for marine systems
  globally; UNEP-WCMC and IUCN 2025).

- cap_area_target:

  `numeric` value denoting the area-based target cap. To avoid setting a
  target cap, a missing (`NA`) value can be specified. Defaults to
  1000000 (i.e., 1,000,000 km²).

- area_units:

  `character` value denoting the unit of measurement for the area-based
  arguments. Defaults to `"km^2"` (i.e., km²).

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

This target setting method was designed to protect species in national-
scale prioritizations (Polak *et al.* 2015). Although it has been
successfully applied to to national-scales (e.g., Polak *et al.* 2016;
Clements *et al.* 2018; ), it may fail to identify meaningful priorities
for prioritizations conducted at smaller or larger geographic scales
(e.g., local or global scales). For example, if this method is applied
to smaller geographic scales, then the resulting prioritizations may
select an overly large percentage of the study area, or be biased
towards over-representing common and widespread species. This is because
the thresholds for defining rare and common features (i.e.,
`rare_area_threshold` and `common_area_threshold`) were originally
developed based on criteria for national-scales. As such, if you working
at a different scale, you may need to calibrate these thresholds based
on the spatial extent of the planning region. Please note that this
function is provided as convenient method to set targets for problems
with a single management zone, and cannot be used for those with
multiple management zones.

## Mathematical formulation

This method involves setting target thresholds based on the spatial
extent of the features. By default, this method identifies rare features
as those with a spatial distribution smaller than 1,000 km² (per
`rare_area_threshold` and `area_units`) and common features as those
with a spatial distribution larger than 10,000 km² (per
`common_area_threshold` and `area_units`). Given this, rare features are
assigned a target threshold of 100% (per `rare_relative_target`), common
features are assigned a target threshold of 10% (per
`common_relative_target`), and features with a spatial distribution that
is between the area-based thresholds used to identify rare and common
features are assigned a target threshold through linear interpolation.
Additionally, following Butchart *et al.* (2015), a cap of 1,000,000 km²
is applied to target thresholds (per `cap_area_threshold` and
`area_units`).

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

Clements HS, Kearney SG, Cook CN (2018) Moving from representation to
persistence: The capacity of Australia's National Reserve System to
support viable populations of mammals. *Diversity and Distributions*,
24: 1231–1241.

Polak T, Watson JEM, Fuller RA, Joseph LN, Martin TG, Possingham HP,
Venter O, Carwardine J (2015) Efficient expansion of global protected
areas requires simultaneous planning for species and ecosystems. *Royal
Society Open Science*, 2: 150107.

Polak T, Watson JEM, Bennett JR, Possingham HP, Fuller RA, Carwardine J
(2016) Balancing ecosystem and threatened species representation in
protected areas and implications for nations achieving global
conservation goals. *Conservation Letters*, 9:438–445.

UNEP-WCMC and IUCN (2025) Protected Planet Report 2024. Cambridge, UK:
UNEP-WCMC and IUCN. Available at \<www.protectedplanet.net\>.

## See also

Other target setting methods:
[`spec_absolute_targets()`](https://prioritizr.net/reference/spec_absolute_targets.md),
[`spec_area_targets()`](https://prioritizr.net/reference/spec_area_targets.md),
[`spec_duran_targets()`](https://prioritizr.net/reference/spec_duran_targets.md),
[`spec_interp_absolute_targets()`](https://prioritizr.net/reference/spec_interp_absolute_targets.md),
[`spec_interp_area_targets()`](https://prioritizr.net/reference/spec_interp_area_targets.md),
[`spec_jung_targets()`](https://prioritizr.net/reference/spec_jung_targets.md),
[`spec_max_targets()`](https://prioritizr.net/reference/spec_max_targets.md),
[`spec_min_targets()`](https://prioritizr.net/reference/spec_min_targets.md),
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

# create problem with Polak et al. (2015) targets
p1 <-
  problem(sim_complex_pu_raster, sim_complex_features) %>%
  add_min_set_objective() %>%
  add_auto_targets(method = spec_polak_targets()) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# solve problem
s1 <- solve(p1)

# plot solution
plot(s1, main = "solution", axes = FALSE)

# }
```
