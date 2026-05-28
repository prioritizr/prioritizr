# Zone names

Extract the names of zones in an object.

## Usage

``` r
zone_names(x, ...)

# S3 method for class 'ConservationProblem'
zone_names(x, ...)

# S3 method for class 'MultiObjConservationProblem'
zone_names(x, ...)

# S3 method for class 'ZonesRaster'
zone_names(x, ...)

# S3 method for class 'ZonesSpatRaster'
zone_names(x, ...)

# S3 method for class 'ZonesCharacter'
zone_names(x, ...)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md),
  [`multi_problem()`](https://prioritizr.net/reference/multi_problem.md),
  or [`Zones()`](https://prioritizr.net/reference/zones.md) object.

- ...:

  not used.

## Value

A `character` vector of zone names.

## Examples

``` r
# \dontrun{
# load data
sim_zones_pu_raster <- get_sim_zones_pu_raster()
sim_zones_features <- get_sim_zones_features()

# print names of zones in a Zones object
print(zone_names(sim_zones_features))
#> [1] "zone_1" "zone_2" "zone_3"
# create problem with multiple zones
p <-
  problem(sim_zones_pu_raster, sim_zones_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
  add_binary_decisions()

# print zone names in problem
print(zone_names(p))
#> [1] "zone_1" "zone_2" "zone_3"

# create two example problems
p1 <-
  problem(sim_zones_pu_raster, sim_zones_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
  add_binary_decisions()

p2 <-
  problem(sim_zones_pu_raster, sim_zones_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
  add_binary_decisions()

# create multi-objective problem
mp <-
  multi_problem(p1, p2) %>%
  add_hier_approach(rel_tol = 0.1, verbose = FALSE) %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# print zone names
print(zone_names(mp))
#> [1] "zone_1" "zone_2" "zone_3"
# }
```
