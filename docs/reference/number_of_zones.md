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

An `integer` number of zones.

## Examples

``` r
# \dontrun{
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

# define budget for multi-objective problem
b <- 0.3 * terra::global(sim_pu_raster, "sum", na.rm = TRUE)[[1]]
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'global': object 'sim_pu_raster' not found

# TODO: example for multi-objective problem
# }
```
