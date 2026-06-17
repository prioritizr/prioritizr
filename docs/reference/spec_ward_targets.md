# Specify targets following Ward *et al.* (2025)

Specify targets based on the methodology outlined by Ward *et al.*
(2025). Briefly, this method involves setting targets based the criteria
for recognizing Critically Endangered species by the International Union
for the Conservation of Nature (IUCN) Red List of Threatened Species
(IUCN 2025). To help prevent widespread features from obscuring
priorities, targets are capped following Butchart *et al.* (2015). This
method was designed for species protection at national-scales. Note that
this function is designed to be used with
[`add_auto_targets()`](https://prioritizr.net/reference/add_auto_targets.md)
and
[`add_group_targets()`](https://prioritizr.net/reference/add_group_targets.md).

## Usage

``` r
spec_ward_targets(status = "CR", cap_area_target = 1e+06, area_units = "km^2")
```

## Arguments

- status:

  `character` value denoting the IUCN Red List threat status used for
  target setting. Available options include `"CR"` (Critically
  Endangered) , `"EN"` (Endangered), and `"VU"` (Vulnerable). Defaults
  to `"CR"`.

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

This target setting method was designed to protect species in a
national-scale prioritizations (Ward *et al.* 2025). Since it was
designed for national-scale prioritizations, it may fail to identify
meaningful priorities for prioritizations conducted at smaller
geographic scales (e.g., national, state-level or county scales). For
example, if this method is applied to smaller geographic scales, then
the resulting prioritizations may select an overly large percentage of
the study area, or be biased towards over-representing common and
widespread species. As such, if you are working at smaller scales, it is
recommended to set thresholds based on that criteria are appropriate to
the spatial extent of the planning region. Please note that this
function is provided as convenient method to set targets for problems
with a single management zone, and cannot be used for those with
multiple management zones.

## Mathematical formulation

This method involves setting target thresholds based on assessment
criteria from the IUCN Red List (IUCN 2025). It is based on the
rationale that protected areas prevent the local extinction of
populations located inside them, and so a protected area system can
safeguard enough of a species' distribution to ensure that the species –
in event that it becomes locally extinct outside of protected areas –
would, at worst, be classified under a particular threat status. In
particular, this method considers criteria related to the size of a
species' spatial distribution (i.e., Criterion B) and population size
reduction (i.e., Criterion A). By default, it considers criteria for the
Critically Endangered threat status and involves setting the target
threshold for a species as 100,000 km² (per subcriterion B1) or 20% (per
subcriterion A2) of its spatial distribution (which ever value is
larger). Additionally, following Butchart *et al.* (2015), a cap of
1,000,000 km² is applied to target thresholds (per `cap_area_threshold`
and `area_units`). By helping to ensure that species would – at a
minimum – meet criteria for being recognized as Critically Endangered,
this method aims to reduce chance that species will become extinct.

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

IUCN (2025) The IUCN Red List of Threatened Species. Version 2025-1.
Available at <https://www.iucnredlist.org>. Accessed on 23 July 2025.

Ward M, Possingham HP, Wintle BA, Woinarski JCZ, Marsh JR, Chapple DG,
Lintermans M, Scheele BC, Whiterod NS, Hoskin CJ, Aska B, Yong C,
Tulloch A, Stewart R, Watson JEM (2025) The estimated cost of preventing
extinction and progressing recovery for Australia's priority threatened
species. *Proceedings of the National Academy of Sciences*, 122:
e2414985122.

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
[`spec_polak_targets()`](https://prioritizr.net/reference/spec_polak_targets.md),
[`spec_pop_size_targets()`](https://prioritizr.net/reference/spec_pop_size_targets.md),
[`spec_relative_targets()`](https://prioritizr.net/reference/spec_relative_targets.md),
[`spec_rl_ecosystem_targets()`](https://prioritizr.net/reference/spec_rl_ecosystem_targets.md),
[`spec_rl_species_targets()`](https://prioritizr.net/reference/spec_rl_species_targets.md),
[`spec_rodrigues_targets()`](https://prioritizr.net/reference/spec_rodrigues_targets.md),
[`spec_rule_targets()`](https://prioritizr.net/reference/spec_rule_targets.md),
[`spec_sreekar_targets()`](https://prioritizr.net/reference/spec_sreekar_targets.md),
[`spec_watson_targets()`](https://prioritizr.net/reference/spec_watson_targets.md),
[`spec_wilson_targets()`](https://prioritizr.net/reference/spec_wilson_targets.md)

## Examples

``` r
# set seed for reproducibility
set.seed(500)

# load data
sim_complex_pu_raster <- get_sim_complex_pu_raster()
sim_complex_features <- get_sim_complex_features()

# create problem with Ward et al. (2025) targets
p1 <-
  problem(sim_complex_pu_raster, sim_complex_features) %>%
  add_min_set_objective() %>%
  add_auto_targets(method = spec_ward_targets()) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# solve problem
s1 <- solve(p1)

# plot solution
plot(s1, main = "solution", axes = FALSE)
```
