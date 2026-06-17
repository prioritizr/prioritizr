# Specify targets based on population size

Specify targets based on the minimum number of individuals for each
feature. Briefly, this method involves using population density data to
set a target threshold for the minimum amount of habitat required to
safeguard a particular number of individuals. To help prevent widespread
features from obscuring priorities, targets are capped following
Butchart *et al.* (2015). Note that this function is designed to be used
with
[`add_auto_targets()`](https://prioritizr.net/reference/add_auto_targets.md)
and
[`add_group_targets()`](https://prioritizr.net/reference/add_group_targets.md).

## Usage

``` r
spec_pop_size_targets(
  pop_size_targets,
  pop_density,
  density_units,
  cap_area_target = 1e+06,
  area_units = "km^2"
)
```

## Arguments

- pop_size_targets:

  `numeric` vector that specifies the minimum population size required
  for each feature. If a single `numeric` value is specified, then all
  features are assigned targets based on the same population size.

- pop_density:

  `numeric` vector that specifies the population density for each
  feature. If a single `numeric` value is specified, then all features
  are assigned targets assuming the same population density. See
  Population density section for more details.

- density_units:

  `character` vector that specifies the area-based units for the
  population density values. For example, units can be used to express
  that population densities are in terms of individuals per hectare
  (`"ha"`), acre (`"acre"`), or km² (`"km^2"`). If a single `character`
  value is specified, then all features are assigned targets assuming
  that population density values are in the same units. See Population
  density section for more details.

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

This target setting method can be used to set targets for species based
on population size thresholds. Many different population size thresholds
– and methods for calculating such thresholds – have been proposed for
guiding conservation decisions (Di Marco *et al.* 2016). For example,
previous work has suggested that the number of individuals for a
population should not fall below 50 individuals to avoid inbreeding
depression, and 500 individuals to reduce genetic drift (reviewed by
Jamieson and Allendorf 2012). Also, the Red List of Threatened Species
by the International Union for the Conservation of Nature has criteria
related to population size, where a species with fewer than 250, 2,500,
or 10,000 individuals are recognized as Critically Endangered,
Endangered, or Vulnerable (respectively) (IUCN 2025). Additionally, the
SAFE index (Clements *et al.* 2011) considers species with fewer than
5,000 individuals to be threatened by extinction (based on Brook *et
al.* 2006; Traill *et al.* 2007, 2010). Furthermore, Hilbers *et al.*
(2017) and Wolff *et al.* (2023) developed methodologies for estimating
species-specific population sizes for protection based on population
growth rates.

## Population density

This method requires population density data expressed as the number of
individuals per unit area. For example, if a species has 200 individuals
per hectare, then this can be specified with `pop_density = 200` and
`density_units = "ha"`. Alternatively, if a species has a population
density where one individual occurs every 10 km², then this can be
specified with `pop_density = 0.1` and `density_units = "km^2"`. Also,
note that population density is assumed to scale linearly with the
values in the feature data. For example, if a planning unit contains 5
km² of habitat for a feature, `pop_density = 200`, and
`density_units = "km^2"`, then the calculations assume that the planning
unit contains 100 individuals for the species. Although the package does
not provide the population density data required to apply this target
setting method, such data can be obtained from published databases
(e.g., Santini *et al.* 2022, 2023, 2024; Witting *et al.* 2024).

## Mathematical formulation

This method involves setting target thresholds based on the amount of
habitat required to safeguard a pre-specified number of individuals. To
express this mathematically, we will define the following terminology.
Let \\f\\ denote the total abundance of a feature (i.e., geographic
range size expressed as km²), \\n\\ denote the minimum number of
individuals that should ideally be represented (per `pop_size_targets`),
and \\d\\ population density of the feature (i.e., number of individuals
per km², per `pop_density` and `density_units`), and \\j\\ the target
cap (expressed as km², per `cap_area_target` and `area_units`). Given
this terminology, the target threshold (\\t\\) for the feature is
calculated as follows. \$\$ t = min(f, j, n \times d)\$\$

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

Brook BW, Traill LW, Bradshaw CJA (2006) Minimum viable population sizes
and global extinction risk are unrelated. *Ecology Letters*, 9:375–382.

Clements GR, Bradshaw CJ, Brook BW, Laurance WF (2011) The SAFE index:
using a threshold population target to measure relative species threat.
*Frontiers in Ecology and the Environment*, 9:521–525.

Di Marco M, Santini L, Visconti P, Mortelliti A, Boitani L, Rondinini C
(2016) Using habitat suitability models to scale up population
persistence targets for global species conservation. *Hystrix, the
Italian Journal of Mammalogy*, 27.

Hilbers JP, Santini L, Visconti P, Schipper AM, Pinto C, Rondinini C,
Huijbregts MAJ (2016) Setting population targets for mammals using body
mass as a predictor of population persistence. *Conservation Biology*,
31:385–393.

Jamieson IG, Allendorf FW (2012) How does the 50/500 rule apply to MVPs?
*Trends in Ecology and Evolution*, 27:578–584.

IUCN (2025) The IUCN Red List of Threatened Species. Version 2025-1.
Available at <https://www.iucnredlist.org>. Accessed on 23 July 2025.

Santini L, Mendez Angarita VY, Karoulis C, Fundarò D, Pranzini N,
Vivaldi C, Zhang T, Zampetti A, Gargano SJ, Mirante D, Paltrinieri L
(2024) TetraDENSITY 2.0—A database of population density estimates in
tetrapods. *Global Ecology and Biogeography*, 33:e13929.

Santini L, Benítez‐López A, Dormann CF, Huijbregts MAJ (2022) Population
density estimates for terrestrial mammal species. *Global Ecology and
Biogeography*, 31:978–994.

Santini L, Tobias JA, Callaghan C, Gallego‐Zamorano J, Benítez‐López A
(2023) Global patterns and predictors of avian population density.
*Global Ecology and Biogeography*, 32:1189—1204.

Traill LW, Brook BW, Frankham RR, Bradshaw CJA (2010) Pragmatic
population viability targets in a rapidly changing world. *Biological
Conservation*, 143:28–34

Traill LW, Bradshaw CJA, Brook BW (2007) Minimum viable population size:
A meta-analysis of 30 years of published estimates. *Biological
Conservation*, 139:159–166.

Witting L (2024) Population dynamic life history models of the birds and
mammals of the world. *Ecological Informatics*, 80:102492.

Wolff NH, Visconti P, Kujala H, Santini L, Hilbers JP, Possingham HP,
Oakleaf JR, Kennedy CM, Kiesecker J, Fargione J, Game ET (2023)
Prioritizing global land protection for population persistence can
double the efficiency of habitat protection for reducing mammal
extinction risk. *One Earth*, 6:1564–1575.

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
[`spec_relative_targets()`](https://prioritizr.net/reference/spec_relative_targets.md),
[`spec_rl_ecosystem_targets()`](https://prioritizr.net/reference/spec_rl_ecosystem_targets.md),
[`spec_rl_species_targets()`](https://prioritizr.net/reference/spec_rl_species_targets.md),
[`spec_rodrigues_targets()`](https://prioritizr.net/reference/spec_rodrigues_targets.md),
[`spec_rule_targets()`](https://prioritizr.net/reference/spec_rule_targets.md),
[`spec_sreekar_targets()`](https://prioritizr.net/reference/spec_sreekar_targets.md),
[`spec_ward_targets()`](https://prioritizr.net/reference/spec_ward_targets.md),
[`spec_watson_targets()`](https://prioritizr.net/reference/spec_watson_targets.md),
[`spec_wilson_targets()`](https://prioritizr.net/reference/spec_wilson_targets.md)

## Examples

``` r
# set seed for reproducibility
set.seed(500)

# load data
sim_complex_pu_raster <- get_sim_complex_pu_raster()
sim_complex_features <- get_sim_complex_features()

# simulate population density data for each feature,
# expressed as number of individuals per km^2
sim_pop_density_per_km2 <- runif(terra::nlyr(sim_complex_features), 10, 1000)

# create base problem
p0 <-
  problem(sim_complex_pu_raster, sim_complex_features) %>%
  add_min_set_objective() %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# create problem with targets to ensure that, at least, 2500 individuals
# for each feature are represented by the solution
p1 <-
  p0 %>%
  add_auto_targets(
    method = spec_pop_size_targets(
      pop_size = 2500,
      pop_density = sim_pop_density_per_km2,
      density_units = "km^2"
    )
  )

# solve problem
s1 <- solve(p1)

# plot solution
plot(s1, main = "solution based on 2500 population targets", axes = FALSE)


# create problem with targets to ensure that a particular number of
# individuals for each feature are represented by the solution

# simulate the number of number of individuals required for each feature
target_pop_sizes <- round(
  runif(terra::nlyr(sim_complex_features), 1000, 5000)
 )

# now, create problem with these targets
p2 <-
  p0 %>%
  add_auto_targets(
    method = spec_pop_size_targets(
      pop_size = target_pop_sizes,
      pop_density = sim_pop_density_per_km2,
      density_units = "km^2"
    )
  )

# solve problem
s2 <- solve(p2)

# plot solution
plot(s2, main = "solution based on varying targets", axes = FALSE)
```
