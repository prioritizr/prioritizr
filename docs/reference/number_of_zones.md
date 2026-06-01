# Number of zones

Extract the number of zones in an object.

## Usage

``` r
number_of_zones(x, ...)

# S3 method for class 'ConservationProblem'
number_of_zones(x, ...)

# S3 method for class 'MultiObjConservationProblem'
number_of_zones(x, ...)

# S3 method for class 'OptimizationProblem'
number_of_zones(x, ...)

# S3 method for class 'ZonesRaster'
number_of_zones(x, ...)

# S3 method for class 'ZonesSpatRaster'
number_of_zones(x, ...)

# S3 method for class 'ZonesCharacter'
number_of_zones(x, ...)
```

## Arguments

- x:

  [`problem()`](https://prioritizr.net/reference/problem.md),
  [`optimization_problem()`](https://prioritizr.net/reference/optimization_problem.md),
  or [`zones()`](https://prioritizr.net/reference/zones.md) object.

- ...:

  not used.

## Value

An `integer` value.

## Examples

``` r
# load data
sim_zones_pu_raster <- get_sim_zones_pu_raster()
sim_zones_features <- get_sim_zones_features()

# print number of zones in a Zones object
print(number_of_zones(sim_zones_features))
#> [1] 3
# create problem with multiple zones
p <-
  problem(sim_zones_pu_raster, sim_zones_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
  add_binary_decisions()

# print number of zones in the problem
print(number_of_zones(p))
#> [1] 3

# create two example problems
mp <-
  multi_problem(
    obj1 =
      problem(sim_zones_pu_raster, sim_zones_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(matrix(0.2, ncol = 3, nrow = 5)) %>%
      add_binary_decisions(),
    obj2 =
      problem(sim_zones_pu_raster, sim_zones_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(matrix(0.1, ncol = 3, nrow = 5)) %>%
      add_binary_decisions()
  )

# print number of zones
print(number_of_zones(mp))
#> [1] 3
```
